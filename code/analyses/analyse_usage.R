# ============================================================================
# analyse_usage.R
# ----------------------------------------------------------------------------
# One-shot HTML usage report generator for TIER.
#
# Reads the SQLite events table produced by usage_logger.R and produces a
# self-contained HTML report covering:
#
#   1. Summary stats (date range, totals)
#   2. Sessions per day
#   3. Hour-of-day usage pattern
#   4. Engagement funnel (sessions reaching Results, view_count distribution)
#   5. Countries modelled (input$region distribution)
#   6. User country of access (currently mostly "local"/"unknown" until proxy)
#   7. Most-adapted interventions (parsed from delta_json)
#   8. Scenario value ranges for top-adapted interventions
#
# USAGE (RStudio):
#   source("analyse_usage.R")
#   generate_report("~/Desktop/tier_usage.sqlite")
#   # Opens the HTML report in your browser automatically.
#
#   # With a custom start cutoff (events before this UTC instant are dropped):
#   generate_report("~/Desktop/tier_usage.sqlite",
#                   cutoff_utc = as.POSIXct("2026-06-12 10:00:00", tz = "UTC"))
#
#   # Bounded window -- keep only events in [start, end] inclusive (UTC).
#   # end_cutoff_utc = NULL (the default) means no upper bound.
#   generate_report("~/Desktop/tier_usage.sqlite",
#                   cutoff_utc     = as.POSIXct("2026-06-12 10:00:00", tz = "UTC"),
#                   end_cutoff_utc = as.POSIXct("2026-07-31 23:59:59", tz = "UTC"))
#
# USAGE (terminal):
#   Rscript analyse_usage.R ~/Desktop/tier_usage.sqlite
#
# An optional output path can be specified:
#   generate_report("~/Desktop/tier_usage.sqlite", "~/Desktop/report.html")
#
# Robustness notes
# ----------------
#   * Each section checks for sufficient data and shows a placeholder if
#     empty. The report renders cleanly even on a freshly-created DB with
#     zero rows.
#   * delta_json parsing is wrapped in tryCatch -- malformed rows are
#     skipped, not fatal.
#   * The report is self-contained (single HTML file) so it can be emailed,
#     shared, or archived without external dependencies.
# ============================================================================

# ---------------------------------------------------------------------------
# Required packages -- print missing list, don't auto-install
# ---------------------------------------------------------------------------
.required_pkgs <- c("DBI", "RSQLite", "jsonlite", "dplyr", "ggplot2",
                    "rmarkdown", "knitr", "scales")

.check_packages <- function() {
  missing <- .required_pkgs[!sapply(.required_pkgs, requireNamespace, quietly = TRUE)]
  if (length(missing) > 0) {
    stop(sprintf(
      "Missing required packages. Install with:\n  install.packages(c(%s))",
      paste(sprintf("'%s'", missing), collapse = ", ")
    ), call. = FALSE)
  }
}

# ---------------------------------------------------------------------------
# Default analysis cutoff: events strictly before this UTC instant are
# excluded from the report. Rows with unparseable (NA) timestamps are
# always kept regardless of cutoff.
#
# 2026-06-12 12:00 Netherlands time = 2026-06-12 10:00 UTC
#   (Netherlands is on CEST = UTC+2 in June.)
# ---------------------------------------------------------------------------
DEFAULT_CUTOFF_UTC <- as.POSIXct("2026-06-12 10:00:00", tz = "UTC")

# ---------------------------------------------------------------------------
# Main entry point. Takes a path to the SQLite file and (optionally) an
# output HTML path and a cutoff timestamp. Returns the path of the
# generated HTML file invisibly.
# ---------------------------------------------------------------------------
generate_report <- function(sqlite_path,
                            output_path    = NULL,
                            cutoff_utc     = DEFAULT_CUTOFF_UTC,
                            end_cutoff_utc = NULL,
                            open_after     = interactive()) {
  .check_packages()
  
  sqlite_path <- normalizePath(sqlite_path, mustWork = TRUE)
  
  # Coerce cutoff to POSIXct in UTC if a string was passed
  if (is.character(cutoff_utc)) {
    cutoff_utc <- as.POSIXct(cutoff_utc, tz = "UTC")
  }
  if (!inherits(cutoff_utc, "POSIXct")) {
    stop("cutoff_utc must be a POSIXct or a parseable character string.",
         call. = FALSE)
  }
  attr(cutoff_utc, "tzone") <- "UTC"
  
  # Same coercion for the optional end cutoff (NULL = no upper bound)
  if (!is.null(end_cutoff_utc)) {
    if (is.character(end_cutoff_utc)) {
      end_cutoff_utc <- as.POSIXct(end_cutoff_utc, tz = "UTC")
    }
    if (!inherits(end_cutoff_utc, "POSIXct")) {
      stop("end_cutoff_utc must be NULL, a POSIXct, or a parseable character string.",
           call. = FALSE)
    }
    attr(end_cutoff_utc, "tzone") <- "UTC"
    if (end_cutoff_utc < cutoff_utc) {
      stop(sprintf(
        "end_cutoff_utc (%s) is before cutoff_utc (%s).",
        format(end_cutoff_utc, "%Y-%m-%d %H:%M"),
        format(cutoff_utc,     "%Y-%m-%d %H:%M")), call. = FALSE)
    }
  }
  
  if (is.null(output_path)) {
    output_path <- file.path(
      dirname(sqlite_path),
      sprintf("tier_usage_report_%s.html", format(Sys.Date(), "%Y%m%d"))
    )
  }
  output_path <- normalizePath(output_path, mustWork = FALSE)
  
  # Read the data
  message("[analyse_usage] Reading: ", sqlite_path)
  df <- .read_events(sqlite_path, cutoff_utc = cutoff_utc,
                     end_cutoff_utc = end_cutoff_utc)
  message(sprintf("[analyse_usage] %d rows loaded (after cutoff filter)", nrow(df)))
  
  # Generate the report -- writes Rmd to a tempfile and knits it
  rmd_path <- tempfile(fileext = ".Rmd")
  writeLines(.report_template(), rmd_path)
  
  message("[analyse_usage] Rendering report ...")
  rmarkdown::render(
    rmd_path,
    output_file = basename(output_path),
    output_dir  = dirname(output_path),
    params = list(events_df      = df,
                  sqlite_path    = sqlite_path,
                  cutoff_utc     = cutoff_utc,
                  end_cutoff_utc = end_cutoff_utc),
    quiet = TRUE
  )
  
  message("[analyse_usage] Report written to: ", output_path)
  
  if (open_after) {
    utils::browseURL(output_path)
  }
  
  invisible(output_path)
}

# ---------------------------------------------------------------------------
# Read the events table. Returns an empty data.frame with the expected
# columns if the DB is empty -- so downstream code can always assume the
# columns exist.
#
# Rows whose parsed timestamp is strictly before cutoff_utc are dropped.
# Rows with NA timestamps (parse failures) are KEPT -- we can't verify
# they are pre-cutoff and would rather over-include than silently lose
# logged events.
# ---------------------------------------------------------------------------
.read_events <- function(sqlite_path,
                         cutoff_utc     = DEFAULT_CUTOFF_UTC,
                         end_cutoff_utc = NULL) {
  con <- DBI::dbConnect(RSQLite::SQLite(), sqlite_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  
  df <- DBI::dbGetQuery(con, "SELECT * FROM events ORDER BY event_id")
  
  # If empty, return a stub with the right columns so analyses don't error
  if (nrow(df) == 0) {
    df <- data.frame(
      event_id = integer(), timestamp = character(),
      session_id = character(), ip_hash = character(),
      user_country = character(), event_type = character(),
      model_country = character(), scenario1_json = character(),
      scenario2_json = character(), baseline_json = character(),
      delta_json = character(), view_count = integer(),
      stringsAsFactors = FALSE
    )
    df$timestamp_posix <- as.POSIXct(character(0), tz = "UTC")
    return(df)
  }
  
  # Parse timestamps into POSIXct (UTC) for downstream use
  df$timestamp_posix <- tryCatch(
    as.POSIXct(df$timestamp, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    error = function(e) rep(as.POSIXct(NA), nrow(df))
  )
  
  # Apply window: keep rows in [cutoff_utc, end_cutoff_utc] (both inclusive),
  # OR rows with NA timestamps. A bare comparison returns NA for NA
  # timestamps, which would drop them when used as a filter index -- hence
  # the explicit is.na() branch. end_cutoff_utc = NULL means no upper bound.
  before    <- nrow(df)
  in_window <- df$timestamp_posix >= cutoff_utc
  if (!is.null(end_cutoff_utc)) {
    in_window <- in_window & df$timestamp_posix <= end_cutoff_utc
  }
  keep <- is.na(df$timestamp_posix) | in_window
  df   <- df[keep, , drop = FALSE]
  n_na <- sum(is.na(df$timestamp_posix))
  message(sprintf(
    "[analyse_usage] Window [%s, %s] UTC: kept %d/%d rows (%d with NA timestamps retained)",
    format(cutoff_utc, "%Y-%m-%d %H:%M"),
    if (is.null(end_cutoff_utc)) "open" else format(end_cutoff_utc, "%Y-%m-%d %H:%M"),
    nrow(df), before, n_na
  ))
  
  df
}

# ---------------------------------------------------------------------------
# The Rmd template. Inlined as a string so analyse_usage.R is a single file.
# Each chunk handles its own "no data" case.
# ---------------------------------------------------------------------------
.report_template <- function() {
  r"---(---
title: "TIER Usage Report"
output:
  html_document:
    self_contained: true
    theme: flatly
    highlight: tango
    toc: true
    toc_float: true
    toc_depth: 2
params:
  events_df: !r data.frame()
  sqlite_path: ""
  cutoff_utc: !r as.POSIXct("1970-01-01", tz = "UTC")
  end_cutoff_utc: !r NULL
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE,
                      results = "asis",
                      fig.width = 8, fig.height = 4)
library(dplyr)
library(ggplot2)
library(jsonlite)
library(scales)

df <- params$events_df

# Helpers
empty_msg <- function(text) {
  cat(sprintf("<p style='color:#888;font-style:italic;'>%s</p>", text))
}

# -----------------------------------------------------------------------
# Delta detection: which intervention fields differ from baseline by more
# than DELTA_THRESHOLD (relative), with a floor to avoid division-by-zero
# for baselines near zero.
#
# Formula: abs(scenario - baseline) / max(abs(baseline), FLOOR) > THRESHOLD
#
# Chosen values:
#   THRESHOLD = 0.05  (5%) -- below this is in the noise of slider drags;
#                            5% is the smallest move that plausibly reflects
#                            decision-maker intent.
#   FLOOR     = 1     -- when baseline is 0 or tiny, require an absolute
#                        change of at least 0.05 (5% of 1).
# -----------------------------------------------------------------------
DELTA_THRESHOLD <- 0.05
DELTA_FLOOR     <- 1

# Parse a JSON string into a named numeric list, dropping non-numeric or
# unparseable fields. Returns named numeric vector or empty named numeric.
parse_inputs_json <- function(j) {
  if (is.na(j) || !nzchar(j)) return(setNames(numeric(0), character(0)))
  tryCatch({
    v <- fromJSON(j)
    if (!is.list(v) || length(v) == 0) return(setNames(numeric(0), character(0)))
    # Keep only numeric scalars
    keep <- vapply(v, function(x) is.numeric(x) && length(x) == 1 && !is.na(x), logical(1))
    if (!any(keep)) return(setNames(numeric(0), character(0)))
    out <- unlist(v[keep])
    out
  }, error = function(e) setNames(numeric(0), character(0)))
}

# For one row of scenario_json + baseline_json, return names of fields
# where the scenario differs materially (per DELTA_THRESHOLD/FLOOR) from
# the baseline.
fields_changed_from_baseline <- function(scenario_json, baseline_json) {
  s <- parse_inputs_json(scenario_json)
  b <- parse_inputs_json(baseline_json)
  common <- intersect(names(s), names(b))
  if (length(common) == 0) return(character(0))
  rel_diff <- abs(s[common] - b[common]) / pmax(abs(b[common]), DELTA_FLOOR)
  common[rel_diff > DELTA_THRESHOLD]
}

# Theme for plots -- clean, decision-maker-friendly
tier_theme <- theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 13),
    axis.title = element_text(size = 11, color = "#444"),
    axis.text = element_text(color = "#444")
  )
```

*Report generated `r format(Sys.time(), "%Y-%m-%d %H:%M %Z")` from `r basename(params$sqlite_path)`*

*Analysis window: events on/after `r format(params$cutoff_utc, "%Y-%m-%d %H:%M")` UTC `r if (is.null(params$end_cutoff_utc)) "" else sprintf("and on/before %s UTC ", format(params$end_cutoff_utc, "%Y-%m-%d %H:%M"))`(inclusive; rows with missing timestamps retained)*

---

## 1. Summary

```{r summary}
if (nrow(df) == 0) {
  empty_msg("No events logged yet.")
} else {
  sessions <- df %>% filter(event_type == "session_start")
  results  <- df %>% filter(event_type == "results_view")
  date_range <- range(df$timestamp_posix, na.rm = TRUE)
  
  summary_tbl <- data.frame(
    Metric = c(
      "Date range",
      "Total sessions",
      "Sessions reaching Results tab",
      "Total results-view events",
      "Distinct session IDs",
      "Distinct IP hashes"
    ),
    Value = c(
      sprintf("%s -- %s",
              format(date_range[1], "%Y-%m-%d"),
              format(date_range[2], "%Y-%m-%d")),
      format(nrow(sessions), big.mark = ","),
      format(length(unique(results$session_id)), big.mark = ","),
      format(nrow(results), big.mark = ","),
      format(length(unique(df$session_id)), big.mark = ","),
      format(length(unique(df$ip_hash[!is.na(df$ip_hash)])), big.mark = ",")
    )
  )
  knitr::kable(summary_tbl, format = "html", table.attr = "class='table table-striped'")
}
```

---

## 2. Sessions per day

```{r sessions_per_day}
sessions <- df %>% filter(event_type == "session_start")

if (nrow(sessions) == 0) {
  empty_msg("No session_start events yet.")
} else {
  daily <- sessions %>%
    mutate(date = as.Date(timestamp_posix)) %>%
    count(date)
  
  # Fill in missing days as zero so the chart is continuous
  if (nrow(daily) >= 2) {
    full_dates <- data.frame(date = seq(min(daily$date), max(daily$date), by = "day"))
    daily <- full_dates %>% left_join(daily, by = "date") %>%
      mutate(n = ifelse(is.na(n), 0, n))
  }
  
  print(
    ggplot(daily, aes(x = date, y = n)) +
      geom_col(fill = "#2563eb", width = 0.7) +
      scale_y_continuous(breaks = scales::pretty_breaks()) +
      labs(x = NULL, y = "Sessions") +
      tier_theme
  )
  
  cat(sprintf("<p style='color:#666;'>Total: %d sessions across %d distinct days. Median per active day: %.0f.</p>",
              sum(daily$n), sum(daily$n > 0), median(daily$n[daily$n > 0])))
}
```

---

## 3. Hour-of-day pattern

When during the day do sessions start? Useful for understanding global vs local usage patterns.

```{r hour_pattern}
if (nrow(sessions) == 0) {
  empty_msg("No session_start events yet.")
} else if (nrow(sessions) < 5) {
  empty_msg(sprintf("Only %d sessions so far -- hour-of-day pattern not informative yet.", nrow(sessions)))
} else {
  hourly <- sessions %>%
    mutate(hour = as.integer(format(timestamp_posix, "%H"))) %>%
    count(hour) %>%
    right_join(data.frame(hour = 0:23), by = "hour") %>%
    mutate(n = ifelse(is.na(n), 0, n)) %>%
    arrange(hour)
  
  print(
    ggplot(hourly, aes(x = hour, y = n)) +
      geom_col(fill = "#2563eb", width = 0.8) +
      scale_x_continuous(breaks = seq(0, 23, by = 3),
                         labels = sprintf("%02d:00", seq(0, 23, by = 3))) +
      scale_y_continuous(breaks = scales::pretty_breaks()) +
      labs(x = "Hour of day (UTC)", y = "Sessions") +
      tier_theme
  )
  cat("<p style='color:#666;'>All timestamps in UTC. Adjust mentally for the time zone(s) you expect users in.</p>")
}
```

---

## 4. Engagement funnel

How deep do users go? `view_count` is the cumulative number of times a session opens the Results Comparison tab. A user who clicks Results once and leaves has `max(view_count) = 1`; one who iterates 5 times has `max(view_count) = 5`.

```{r engagement}
results <- df %>% filter(event_type == "results_view")

if (nrow(sessions) == 0) {
  empty_msg("No sessions to analyse yet.")
} else {
  n_sessions <- length(unique(sessions$session_id))
  n_reached <- length(unique(results$session_id))
  pct_reached <- if (n_sessions > 0) 100 * n_reached / n_sessions else 0
  
  cat(sprintf("<p><strong>%d / %d sessions (%.0f%%)</strong> opened the Results Comparison tab.</p>",
              n_reached, n_sessions, pct_reached))
  
  if (n_reached == 0) {
    empty_msg("No results_view events yet.")
  } else {
    depth <- results %>%
      group_by(session_id) %>%
      summarise(max_views = max(view_count, na.rm = TRUE), .groups = "drop") %>%
      count(max_views)
    
    print(
      ggplot(depth, aes(x = factor(max_views), y = n)) +
        geom_col(fill = "#10b981", width = 0.7) +
        labs(x = "Number of times Results tab opened per session",
             y = "Sessions") +
        tier_theme
    )
    
    avg_depth <- mean(results %>% group_by(session_id) %>%
                      summarise(m = max(view_count, na.rm = TRUE)) %>% pull(m))
    cat(sprintf("<p style='color:#666;'>Average iteration depth (among sessions reaching Results): <strong>%.1f</strong> views.</p>",
                avg_depth))
  }
}
```

---

## 5. Countries being modelled

Which country profiles do users select in the app? This is the answer to "what is TIER being used for".

```{r model_country}
results <- df %>% filter(event_type == "results_view")

if (nrow(results) == 0) {
  empty_msg("No results_view events yet.")
} else if (all(is.na(results$model_country))) {
  empty_msg("No model_country values recorded yet.")
} else {
  countries <- results %>%
    filter(!is.na(model_country), nzchar(model_country)) %>%
    count(model_country, sort = TRUE)
  
  print(
    ggplot(countries, aes(x = reorder(model_country, n), y = n)) +
      geom_col(fill = "#7c3aed", width = 0.7) +
      coord_flip() +
      labs(x = NULL, y = "Results-view events") +
      tier_theme
  )
  
  knitr::kable(countries, col.names = c("Country", "Views"),
               format = "html", table.attr = "class='table table-striped'")
}
```

---

## 6. User country of access

```{r user_country, results="asis"}
if (nrow(df) == 0) {
  empty_msg("No data yet.")
} else {
  # Resolve per-session country: for each session, prefer a real country
  # from any results_view row over the "unknown" written on session_start.
  # "local" (old pre-browser-JS rows) and "unknown" are both treated as
  # unresolved.
  resolved <- df %>%
    mutate(country_clean = ifelse(user_country %in% c("local", "unknown"),
                                  NA_character_, user_country)) %>%
    group_by(session_id) %>%
    summarise(
      resolved_country = {
        non_na <- country_clean[!is.na(country_clean)]
        if (length(non_na) > 0) non_na[1] else "unknown"
      },
      .groups = "drop"
    )
  
  uc <- resolved %>%
    count(resolved_country, sort = TRUE)
  
  # Print explicitly so the kable HTML is emitted at this point in the
  # output stream. Bare kable() at chunk end relies on auto-print, which
  # doesn't reliably interact with later cat() calls under results='asis'.
  cat(knitr::kable(uc, col.names = c("Country", "Sessions"),
                   format = "html", table.attr = "class='table table-striped'"))
  
  resolved_pct <- 100 * sum(uc$n[uc$resolved_country != "unknown"]) / sum(uc$n)
  cat(sprintf("<p style='color:#666;'>Resolved for %.0f%% of sessions (%d of %d). ",
              resolved_pct,
              sum(uc$n[uc$resolved_country != "unknown"]),
              sum(uc$n)))
  cat("Resolution uses a browser-side ipapi.co lookup; sessions show ",
      "'unknown' when the lookup is blocked (ad-blockers, strict CSP, or ",
      "session ended before fetch completed). Country names follow ipapi.co's ",
      "convention -- some carry the article (e.g. 'The Netherlands').</p>")
}
```

---

## 7. Most-adapted interventions

Which intervention inputs do users most frequently move away from their stated baseline? Each `results_view` row carries both `baseline_json` (the user's Baseline tab values) and `scenario1_json`/`scenario2_json` (the Scenarios tab values). A field counts as "adapted" when the scenario value differs from the baseline value by more than **5%** (relative, with a floor of 1 to handle baselines near zero).

Fields are prefixed `s1_` (scenario 1) and `s2_` (scenario 2). The first chart collapses across scenarios; the second keeps them separate.

```{r delta_fields}
results <- df %>% filter(event_type == "results_view")

if (nrow(results) == 0) {
  empty_msg("No results_view events yet.")
} else {
  # For each row, list fields where each scenario differs from baseline
  s1_changes <- lapply(seq_len(nrow(results)), function(i) {
    paste0("s1_", fields_changed_from_baseline(
      results$scenario1_json[i], results$baseline_json[i]))
  })
  s2_changes <- lapply(seq_len(nrow(results)), function(i) {
    paste0("s2_", fields_changed_from_baseline(
      results$scenario2_json[i], results$baseline_json[i]))
  })
  
  all_fields <- c(unlist(s1_changes), unlist(s2_changes))
  all_fields <- all_fields[all_fields != "s1_" & all_fields != "s2_"]
  
  if (length(all_fields) == 0) {
    empty_msg("No interventions have been moved >5% from baseline in any logged session yet.")
  } else {
    # Collapsed across scenarios
    collapsed <- gsub("^s[12]_", "", all_fields)
    coll_tbl <- as.data.frame(sort(table(collapsed), decreasing = TRUE))
    names(coll_tbl) <- c("Intervention", "Times adapted")
    
    cat("<h4>Combined across scenarios</h4>")
    print(
      ggplot(head(coll_tbl, 15), aes(x = reorder(Intervention, `Times adapted`),
                                     y = `Times adapted`)) +
        geom_col(fill = "#dc2626", width = 0.7) +
        coord_flip() +
        labs(x = NULL, y = "Times scenario differed >5% from baseline") +
        tier_theme
    )
    
    
  }
}
```

---

## 8. Scale-down vs scale-up of top adapted interventions

For each of Section 7's top-10 most-adapted interventions, how often did users move it *down* from baseline vs *up*? Only adaptations >5% from baseline are counted (same rule as Section 7); rows where the user left the field at baseline are excluded. Scenarios 1 and 2 are pooled. Zero-baseline fields count any non-zero scenario as "up".

```{r direction_split}
results <- df %>% filter(event_type == "results_view")

if (nrow(results) == 0) {
  empty_msg("No results_view events yet.")
} else {
  # Re-derive top-10 from scenario-vs-baseline diffs (same rule as section 7)
  s1_changes <- lapply(seq_len(nrow(results)), function(i) {
    fields_changed_from_baseline(results$scenario1_json[i], results$baseline_json[i])
  })
  s2_changes <- lapply(seq_len(nrow(results)), function(i) {
    fields_changed_from_baseline(results$scenario2_json[i], results$baseline_json[i])
  })
  all_changed <- c(unlist(s1_changes), unlist(s2_changes))

  if (length(all_changed) == 0) {
    empty_msg("No interventions adapted >5% from baseline yet -- nothing to summarise.")
  } else {
    top10 <- head(names(sort(table(all_changed), decreasing = TRUE)), 10)

    # Parse a single field from one row's JSON into a scalar numeric, or NA.
    parse_field <- function(j, fld) {
      tryCatch({
        v <- fromJSON(j)
        if (fld %in% names(v) && is.numeric(v[[fld]]) && length(v[[fld]]) == 1) v[[fld]] else NA_real_
      }, error = function(e) NA_real_)
    }

    # For one field, count adapted (>5% from baseline) rows where the
    # scenario went DOWN vs UP. Pools s1 and s2.
    direction_for_field <- function(fld) {
      bv <- vapply(results$baseline_json,  parse_field, numeric(1), fld = fld)
      s1v <- vapply(results$scenario1_json, parse_field, numeric(1), fld = fld)
      s2v <- vapply(results$scenario2_json, parse_field, numeric(1), fld = fld)

      # Only adapted (>5%) rows count toward direction
      rows_s1 <- !is.na(bv) & !is.na(s1v) &
                 (abs(s1v - bv) / pmax(abs(bv), DELTA_FLOOR) > DELTA_THRESHOLD)
      rows_s2 <- !is.na(bv) & !is.na(s2v) &
                 (abs(s2v - bv) / pmax(abs(bv), DELTA_FLOOR) > DELTA_THRESHOLD)

      b_all <- c(bv[rows_s1], bv[rows_s2])
      s_all <- c(s1v[rows_s1], s2v[rows_s2])

      # Down = scenario < baseline; Up = scenario > baseline.
      # Zero-baseline edge: scenario > 0 -> up; scenario == 0 wouldn't pass
      # the >5% gate when bv==0 (abs diff must exceed 0.05 * FLOOR = 0.05).
      n_down <- sum(s_all < b_all)
      n_up   <- sum(s_all > b_all)
      c(n_down = n_down, n_up = n_up)
    }

    rows <- lapply(top10, function(fld) {
      d <- direction_for_field(fld)
      n_tot <- d[["n_down"]] + d[["n_up"]]
      if (n_tot == 0) return(NULL)
      data.frame(
        Intervention = fld,
        N            = n_tot,
        `N down`     = d[["n_down"]],
        `N up`       = d[["n_up"]],
        `% down`     = round(100 * d[["n_down"]] / n_tot, 0),
        `% up`       = round(100 * d[["n_up"]]   / n_tot, 0),
        check.names = FALSE
      )
    })
    rows <- rows[!vapply(rows, is.null, logical(1))]

    if (length(rows) == 0) {
      empty_msg("None of the top-10 fields had a directional adaptation registered.")
    } else {
      dir_df <- do.call(rbind, rows)
      dir_df <- dir_df[order(-dir_df$N), , drop = FALSE]
      rownames(dir_df) <- NULL

      cat(knitr::kable(dir_df, format = "html",
                       table.attr = "class='table table-striped'"))

      cat("<p style='color:#666;'>",
          "N = number of (results_view row, scenario) pairs where the field was adapted >5% from baseline. ",
          "Rows are ordered by N descending. Percentages may not sum to exactly 100 due to rounding.</p>")
    }
  }
}
```

---

<hr>
<p style="color:#888;font-size:0.85em;">
TIER usage report. Generated from <code>`r params$sqlite_path`</code>.
Analysis window: <code>`r format(params$cutoff_utc, "%Y-%m-%d %H:%M")` UTC</code> to <code>`r if (is.null(params$end_cutoff_utc)) "open" else format(params$end_cutoff_utc, "%Y-%m-%d %H:%M")`</code>.
Re-run <code>generate_report()</code> after pulling a fresh SQLite snapshot.
</p>
)---"
}

# ---------------------------------------------------------------------------
# If sourced from terminal with Rscript, run with positional args.
#   Rscript analyse_usage.R <sqlite> [<output.html>] [<cutoff_utc>]
# cutoff_utc is parsed as "YYYY-MM-DD HH:MM:SS" in UTC.
# ---------------------------------------------------------------------------
if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) == 0) {
    cat("Usage: Rscript analyse_usage.R <path-to-usage.sqlite> [<output.html>] [<cutoff_utc>] [<end_cutoff_utc>]\n")
    quit(save = "no", status = 1)
  }
  sqlite_path <- args[1]
  output_path <- if (length(args) >= 2 && nzchar(args[2])) args[2] else NULL
  cutoff_utc  <- if (length(args) >= 3 && nzchar(args[3])) {
    as.POSIXct(args[3], tz = "UTC")
  } else {
    DEFAULT_CUTOFF_UTC
  }
  end_cutoff_utc <- if (length(args) >= 4 && nzchar(args[4])) {
    as.POSIXct(args[4], tz = "UTC")
  } else {
    NULL
  }
  generate_report(sqlite_path, output_path,
                  cutoff_utc     = cutoff_utc,
                  end_cutoff_utc = end_cutoff_utc,
                  open_after     = FALSE)
}