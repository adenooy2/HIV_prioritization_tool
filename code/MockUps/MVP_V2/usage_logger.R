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
# Main entry point. Takes a path to the SQLite file and (optionally) an
# output HTML path. Returns the path of the generated HTML file invisibly.
# ---------------------------------------------------------------------------
generate_report <- function(sqlite_path,
                            output_path = NULL,
                            open_after = interactive()) {
  .check_packages()
  
  sqlite_path <- normalizePath(sqlite_path, mustWork = TRUE)
  
  if (is.null(output_path)) {
    output_path <- file.path(
      dirname(sqlite_path),
      sprintf("tier_usage_report_%s.html", format(Sys.Date(), "%Y%m%d"))
    )
  }
  output_path <- normalizePath(output_path, mustWork = FALSE)
  
  # Read the data
  message("[analyse_usage] Reading: ", sqlite_path)
  df <- .read_events(sqlite_path)
  message(sprintf("[analyse_usage] %d rows loaded", nrow(df)))
  
  # Generate the report -- writes Rmd to a tempfile and knits it
  rmd_path <- tempfile(fileext = ".Rmd")
  writeLines(.report_template(), rmd_path)
  
  message("[analyse_usage] Rendering report ...")
  rmarkdown::render(
    rmd_path,
    output_file = basename(output_path),
    output_dir = dirname(output_path),
    params = list(events_df = df, sqlite_path = sqlite_path),
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
# ---------------------------------------------------------------------------
.read_events <- function(sqlite_path) {
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
  }
  
  # Parse timestamps into POSIXct (UTC) for downstream use
  df$timestamp_posix <- tryCatch(
    as.POSIXct(df$timestamp, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    error = function(e) rep(as.POSIXct(NA), nrow(df))
  )
  
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

# Initialise top-10 list so downstream sections (8a, 8b) can detect it
# even when there are zero results_view rows.
section7_top10 <- character(0)

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
    section7_top10 <- character(0)  # so downstream sections can detect empty
  } else {
    # Collapsed across scenarios
    collapsed <- gsub("^s[12]_", "", all_fields)
    coll_tbl <- as.data.frame(sort(table(collapsed), decreasing = TRUE))
    names(coll_tbl) <- c("Intervention", "Times adapted")
    
    # Top-10 list shared with sections 8a and 8b -- ensures all three views
    # talk about the same set of interventions
    section7_top10 <- as.character(head(coll_tbl$Intervention, 10))
    
    cat("<h4>Combined across scenarios</h4>")
    print(
      ggplot(head(coll_tbl, 15), aes(x = reorder(Intervention, `Times adapted`),
                                     y = `Times adapted`)) +
        geom_col(fill = "#dc2626", width = 0.7) +
        coord_flip() +
        labs(x = NULL, y = "Times scenario differed >5% from baseline") +
        tier_theme
    )
    
    cat("<h4>Split by scenario</h4>")
    by_scen <- as.data.frame(sort(table(all_fields), decreasing = TRUE))
    names(by_scen) <- c("Field", "Times adapted")
    cat(knitr::kable(by_scen, format = "html",
                     table.attr = "class='table table-striped'"))
  }
}
```

---

## 8a. How much do users move adapted interventions?

For each of Section 7's **top 10 most-adapted interventions**, the table below summarises what scenario-vs-baseline change users actually applied — i.e. the size of the move, not the absolute value set. Only adaptations >5% from baseline are counted; rows where the user left a field at baseline are excluded.

Two tables, because the math differs:

- **Fields with non-zero baseline**: reports the multiplier (`scenario / baseline`). Median of 2.0 means users typically doubled it; 0.5 means halved.
- **Fields with zero baseline**: multiplier is undefined; the table reports the absolute scenario value the user set instead.

```{r multiplier_dist}
results <- df %>% filter(event_type == "results_view")

if (length(section7_top10) == 0 || nrow(results) == 0) {
  empty_msg("No adapted interventions yet -- no multiplier data to summarise.")
} else {
  # For each top-10 field, gather all (baseline, scenario) pairs where the
  # scenario was adapted >5%. Combine s1 and s2 -- the two scenarios are
  # both "users exploring" and pooling them is the most informative cut at
  # this stage.
  parse_field <- function(j, fld) {
    tryCatch({
      v <- fromJSON(j)
      if (fld %in% names(v) && is.numeric(v[[fld]]) && length(v[[fld]]) == 1) v[[fld]] else NA_real_
    }, error = function(e) NA_real_)
  }
  
  pairs_for_field <- function(fld) {
    bv <- vapply(results$baseline_json, parse_field, numeric(1), fld = fld)
    s1v <- vapply(results$scenario1_json, parse_field, numeric(1), fld = fld)
    s2v <- vapply(results$scenario2_json, parse_field, numeric(1), fld = fld)
    
    # Build a single 2-column matrix of (baseline, scenario_value) for any
    # row where adaptation registered (>5% relative diff)
    rows_s1 <- !is.na(bv) & !is.na(s1v) &
               (abs(s1v - bv) / pmax(abs(bv), DELTA_FLOOR) > DELTA_THRESHOLD)
    rows_s2 <- !is.na(bv) & !is.na(s2v) &
               (abs(s2v - bv) / pmax(abs(bv), DELTA_FLOOR) > DELTA_THRESHOLD)
    
    data.frame(
      baseline = c(bv[rows_s1], bv[rows_s2]),
      scenario = c(s1v[rows_s1], s2v[rows_s2])
    )
  }
  
  # Split top10 into "always-zero-baseline" and "non-zero-baseline" cases.
  # If a field's baseline is sometimes zero and sometimes not, it ends up
  # in the non-zero bucket -- the zero rows just drop from the multiplier
  # calc.
  nonzero_rows <- list()
  zero_rows <- list()
  
  for (fld in section7_top10) {
    pairs <- pairs_for_field(fld)
    if (nrow(pairs) == 0) next
    
    nz <- pairs[pairs$baseline != 0, , drop = FALSE]
    zr <- pairs[pairs$baseline == 0, , drop = FALSE]
    
    if (nrow(nz) > 0) {
      mults <- nz$scenario / nz$baseline
      nonzero_rows[[fld]] <- data.frame(
        Intervention = fld,
        N            = nrow(nz),
        `Min mult`   = round(min(mults), 2),
        `Median mult` = round(median(mults), 2),
        `Max mult`   = round(max(mults), 2),
        check.names = FALSE
      )
    }
    if (nrow(zr) > 0) {
      vals <- zr$scenario
      zero_rows[[fld]] <- data.frame(
        Intervention = fld,
        N            = nrow(zr),
        `Min value`  = round(min(vals), 1),
        `Median value` = round(median(vals), 1),
        `Max value`  = round(max(vals), 1),
        check.names = FALSE
      )
    }
  }
  
  if (length(nonzero_rows) > 0) {
    cat("<h4>Adapted from a non-zero baseline (multiplier)</h4>")
    nz_df <- do.call(rbind, nonzero_rows)
    rownames(nz_df) <- NULL
    cat(knitr::kable(nz_df, format = "html",
                     table.attr = "class='table table-striped'"))
  }
  
  if (length(zero_rows) > 0) {
    cat("<h4>Adapted from a zero baseline (absolute scenario value)</h4>")
    z_df <- do.call(rbind, zero_rows)
    rownames(z_df) <- NULL
    cat(knitr::kable(z_df, format = "html",
                     table.attr = "class='table table-striped'"))
  }
  
  if (length(nonzero_rows) == 0 && length(zero_rows) == 0) {
    empty_msg("No adaptation pairs collected for the top-10 fields.")
  }
  
  cat("<p style='color:#666;'>N = number of (results_view row, scenario) pairs where adaptation registered.</p>")
}
```

---

## 8b. Where are these interventions being explored?

For the **same top 10 interventions** as in 8a, the table below shows how many *sessions* in each country adapted each one at least once. A session is counted once per intervention per country, even if it adapted the field across multiple results_view events.

The table is sorted by session count, so the most active (country, intervention) pairs sit at the top.

```{r country_intervention}
results <- df %>% filter(event_type == "results_view")

if (length(section7_top10) == 0 || nrow(results) == 0) {
  empty_msg("No adapted interventions yet -- no country breakdown to show.")
} else {
  # For each row, compute the set of adapted fields (collapsed across s1/s2).
  # Then for each (session_id, model_country, field) triple in our top-10,
  # we count it once.
  rows_with_changes <- lapply(seq_len(nrow(results)), function(i) {
    s1 <- fields_changed_from_baseline(results$scenario1_json[i], results$baseline_json[i])
    s2 <- fields_changed_from_baseline(results$scenario2_json[i], results$baseline_json[i])
    unique(c(s1, s2))
  })
  
  # Build a long data frame: one row per (session_id, model_country, field)
  long_rows <- list()
  for (i in seq_len(nrow(results))) {
    fields_i <- rows_with_changes[[i]]
    fields_i <- fields_i[fields_i %in% section7_top10]
    if (length(fields_i) == 0) next
    long_rows[[i]] <- data.frame(
      session_id = results$session_id[i],
      country    = results$model_country[i],
      field      = fields_i,
      stringsAsFactors = FALSE
    )
  }
  
  if (length(long_rows) == 0) {
    empty_msg("None of the top-10 fields have country-attributed adaptations yet.")
  } else {
    long_df <- do.call(rbind, long_rows)
    long_df <- long_df[!is.na(long_df$country) & nzchar(long_df$country), ]
    
    if (nrow(long_df) == 0) {
      empty_msg("No country information attached to adapted-intervention rows.")
    } else {
      # Distinct (session, country, field) triples -- so a session that
      # adapted PrEP across 5 results_view rows in Zambia counts once.
      distinct_triples <- unique(long_df[, c("session_id", "country", "field")])
      
      # Count sessions per (country, field) pair
      pair_counts <- as.data.frame(
        table(country = distinct_triples$country,
              intervention = distinct_triples$field)
      )
      pair_counts <- pair_counts[pair_counts$Freq > 0, , drop = FALSE]
      pair_counts <- pair_counts[order(-pair_counts$Freq), , drop = FALSE]
      rownames(pair_counts) <- NULL
      
      cat(knitr::kable(pair_counts,
                       col.names = c("Country (modelled)", "Intervention", "Sessions"),
                       format = "html",
                       table.attr = "class='table table-striped'"))
      
      cat("<p style='color:#666;'>",
          sprintf("%d distinct (country, intervention) pairs across %d sessions and %d countries.</p>",
                  nrow(pair_counts),
                  length(unique(distinct_triples$session_id)),
                  length(unique(distinct_triples$country))))
    }
  }
}
```

---

<hr>
<p style="color:#888;font-size:0.85em;">
TIER usage report. Generated from <code>`r params$sqlite_path`</code>.
Re-run <code>generate_report()</code> after pulling a fresh SQLite snapshot.
</p>
)---"
}

# ---------------------------------------------------------------------------
# If sourced from terminal with Rscript, run with positional args.
# ---------------------------------------------------------------------------
if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) == 0) {
    cat("Usage: Rscript analyse_usage.R <path-to-usage.sqlite> [<output.html>]\n")
    quit(save = "no", status = 1)
  }
  sqlite_path <- args[1]
  output_path <- if (length(args) >= 2) args[2] else NULL
  generate_report(sqlite_path, output_path, open_after = FALSE)
}