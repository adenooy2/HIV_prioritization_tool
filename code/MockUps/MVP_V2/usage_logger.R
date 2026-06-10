# ============================================================================
# usage_logger.R
# ----------------------------------------------------------------------------
# Lightweight usage logging for TIER.
#
# Logs two event types to a local SQLite database:
#   1. session_start  -- one row per Shiny session, with hashed IP and
#                        geo-resolved user country (via ipapi.co).
#   2. results_view   -- one row each time the user opens the Results tab.
#                        Captures the country being modelled (input$region),
#                        the full state of scenario1 and scenario2 inputs,
#                        and the delta from a first-view baseline snapshot.
#
# Design principles
# -----------------
#   * Logger MUST NEVER crash the app. Every disk write, every HTTP call,
#     every JSON encode is wrapped in tryCatch. Failures are silent.
#   * Logic file (Mock-Up_logic_V2.R) is untouched. Hooks live only in the
#     interface file.
#   * SQLite is concurrent-safe -- multiple Shiny sessions can write
#     simultaneously without corrupting the file (unlike CSV).
#   * IP addresses are hashed with a salt before storage. Raw IPs are never
#     written to disk.
#   * Geo lookup (ipapi.co) has a 2s timeout. If it fails or times out,
#     user_country is recorded as "unknown" and the app continues normally.
#
# Storage location
# ----------------
# The logger tries paths in this order and uses the first that's writable:
#   1. /var/log/tier/usage.sqlite        (Linux convention; needs write perms)
#   2. ~/tier_usage.sqlite                (user home, always writable)
#   3. tempdir()/tier_usage.sqlite        (last-resort fallback)
#
# The chosen path is printed to the Shiny log at startup so the admin knows
# where to find the file for SCP retrieval.
#
# Schema
# ------
# Single flat `events` table -- avoids joins for analysis and keeps schema
# evolution simple. Column names are explicit so the future admin dashboard
# can build SQL directly against this schema.
#
#   event_id        INTEGER PRIMARY KEY AUTOINCREMENT
#   timestamp       TEXT     -- ISO 8601 UTC
#   session_id      TEXT     -- hash of session$token (stable within session)
#   ip_hash         TEXT     -- salted SHA-256 of client IP
#   user_country    TEXT     -- from ipapi.co, "unknown" on failure
#   event_type      TEXT     -- "session_start" | "results_view"
#   model_country   TEXT     -- input$region; NULL for session_start
#   scenario1_json  TEXT     -- JSON of all intervention values, scenario 1
#   scenario2_json  TEXT     -- JSON of all intervention values, scenario 2
#   baseline_json   TEXT     -- JSON of baseline input values
#   delta_json      TEXT     -- JSON: which fields differ from first-view baseline
#   view_count      INTEGER  -- N-th time the results tab opened in this session
#
# ============================================================================

suppressWarnings({
  library(DBI)
  library(RSQLite)
  library(digest)
  library(jsonlite)
  library(httr)
})

# ---------------------------------------------------------------------------
# Module-level state (set by init_log_db)
# ---------------------------------------------------------------------------
.TIER_LOG_PATH <- NULL
.TIER_LOG_SALT <- NULL   # salt for IP hashing; generated once per server start

# ---------------------------------------------------------------------------
# Initialise the log DB. Call once at app startup (top of server function or
# at file source-time).
#
# Returns the path actually used, or NULL if all candidates failed (in which
# case all subsequent log_* calls are no-ops).
# ---------------------------------------------------------------------------
init_log_db <- function(candidate_paths = c("/var/log/tier/usage.sqlite",
                                            path.expand("~/tier_usage.sqlite"),
                                            file.path(tempdir(), "tier_usage.sqlite"))) {
  
  # Generate per-server-start salt. Restarting the server rotates the salt,
  # which means the same IP gets a different hash across restarts. This is
  # intentional -- it limits cross-session linkage.
  .TIER_LOG_SALT <<- digest(paste(Sys.time(), Sys.getpid(), runif(1)),
                            algo = "sha256")
  
  for (p in candidate_paths) {
    result <- tryCatch({
      dir.create(dirname(p), recursive = TRUE, showWarnings = FALSE)
      con <- dbConnect(SQLite(), p)
      dbExecute(con, "
        CREATE TABLE IF NOT EXISTS events (
          event_id        INTEGER PRIMARY KEY AUTOINCREMENT,
          timestamp       TEXT NOT NULL,
          session_id      TEXT NOT NULL,
          ip_hash         TEXT,
          user_country    TEXT,
          event_type      TEXT NOT NULL,
          model_country   TEXT,
          scenario1_json  TEXT,
          scenario2_json  TEXT,
          baseline_json   TEXT,
          delta_json      TEXT,
          view_count      INTEGER
        )
      ")
      # Index for common queries
      dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_session ON events(session_id)")
      dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_event_type ON events(event_type)")
      dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_timestamp ON events(timestamp)")
      dbDisconnect(con)
      p
    }, error = function(e) NULL)
    
    if (!is.null(result)) {
      .TIER_LOG_PATH <<- result
      message(sprintf("[usage_logger] Logging to: %s", result))
      return(result)
    }
  }
  
  warning("[usage_logger] Could not initialise log DB at any candidate path. ",
          "Logging will be disabled for this server session.")
  return(NULL)
}

# ---------------------------------------------------------------------------
# Hash an IP address with the server-session salt.
# Returns NA_character_ if ip is NULL/NA/empty.
# ---------------------------------------------------------------------------
hash_ip <- function(ip) {
  if (is.null(ip) || is.na(ip) || !nzchar(ip)) return(NA_character_)
  if (is.null(.TIER_LOG_SALT)) return(NA_character_)
  digest(paste0(.TIER_LOG_SALT, ip), algo = "sha256")
}

# ---------------------------------------------------------------------------
# Extract client IP from Shiny session.
# Tries X-Forwarded-For first (for reverse-proxy setups), falls back to
# REMOTE_ADDR. If a comma-separated chain is in X-F-F, takes the first
# (which is the originating client, per HTTP convention).
# ---------------------------------------------------------------------------
extract_client_ip <- function(session) {
  result <- tryCatch({
    xff <- session$request$HTTP_X_FORWARDED_FOR
    if (!is.null(xff) && nzchar(xff)) {
      # First IP in chain = originating client
      return(trimws(strsplit(xff, ",", fixed = TRUE)[[1]][1]))
    }
    session$request$REMOTE_ADDR %||% NA_character_
  }, error = function(e) NA_character_)
  result
}

# Local null-coalesce so the file is self-contained (interface defines its
# own %||%, but the logger may run in test contexts where it isn't defined).
`%||%` <- function(x, y) if (is.null(x)) y else x

# ---------------------------------------------------------------------------
# Resolve an IP to a country using ipapi.co. Synchronous with a hard 2-second
# timeout. Returns "unknown" on any failure (timeout, non-200, parse error,
# missing field, network error).
#
# Free tier (no key): ~1,000 requests/day. If the server's IP gets
# rate-limited, all subsequent calls return "unknown" until reset.
# This is acceptable -- the app must never block on geo lookup.
# ---------------------------------------------------------------------------
resolve_country <- function(ip) {
  if (is.null(ip) || is.na(ip) || !nzchar(ip)) return("unknown")
  # Skip private/loopback ranges -- ipapi.co would return an error anyway
  if (grepl("^(127\\.|10\\.|192\\.168\\.|172\\.(1[6-9]|2[0-9]|3[01])\\.)", ip)) {
    return("local")
  }
  
  tryCatch({
    resp <- httr::GET(
      sprintf("https://ipapi.co/%s/country_name/", ip),
      httr::timeout(2)
    )
    if (httr::status_code(resp) != 200) return("unknown")
    country <- httr::content(resp, as = "text", encoding = "UTF-8")
    country <- trimws(country)
    if (!nzchar(country) || grepl("error|Error", country)) return("unknown")
    country
  }, error = function(e) "unknown")
}

# ---------------------------------------------------------------------------
# Write a single row to the events table. Silent on failure.
# ---------------------------------------------------------------------------
.write_event <- function(row) {
  if (is.null(.TIER_LOG_PATH)) return(invisible(NULL))
  
  tryCatch({
    con <- dbConnect(SQLite(), .TIER_LOG_PATH)
    on.exit(dbDisconnect(con), add = TRUE)
    
    dbExecute(con,
              "INSERT INTO events (
         timestamp, session_id, ip_hash, user_country, event_type,
         model_country, scenario1_json, scenario2_json, baseline_json,
         delta_json, view_count
       ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)",
              params = list(
                row$timestamp, row$session_id, row$ip_hash, row$user_country,
                row$event_type, row$model_country, row$scenario1_json,
                row$scenario2_json, row$baseline_json, row$delta_json,
                row$view_count
              )
    )
  }, error = function(e) {
    # Silent. We must never crash the user's session.
    message(sprintf("[usage_logger] Write failed: %s", e$message))
  })
  invisible(NULL)
}

# ---------------------------------------------------------------------------
# Log a session_start event.
#
# session_id : stable hash of session$token
# ip         : raw IP (will be hashed internally)
# user_country : "unknown" if not yet resolved; can be updated later by
#                another row if you want to resolve asynchronously
# ---------------------------------------------------------------------------
log_session_start <- function(session_id, ip = NA_character_,
                              user_country = "unknown") {
  .write_event(list(
    timestamp      = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    session_id     = session_id,
    ip_hash        = hash_ip(ip),
    user_country   = user_country,
    event_type     = "session_start",
    model_country  = NA_character_,
    scenario1_json = NA_character_,
    scenario2_json = NA_character_,
    baseline_json  = NA_character_,
    delta_json     = NA_character_,
    view_count     = NA_integer_
  ))
}

# ---------------------------------------------------------------------------
# Compute the JSON-encoded delta between two named lists. Returns a JSON
# string of {field: [from, to]} for fields where the values differ.
#
# `baseline` is the snapshot taken on first results-view; `current` is the
# current state. If baseline is NULL (i.e. this IS the first view), returns
# an empty JSON object "{}".
# ---------------------------------------------------------------------------
compute_delta <- function(current, baseline) {
  if (is.null(baseline)) return("{}")
  if (is.null(current))  return("{}")
  
  tryCatch({
    changed <- list()
    all_keys <- union(names(baseline), names(current))
    for (k in all_keys) {
      bv <- baseline[[k]]
      cv <- current[[k]]
      # Treat NULL/NA as missing
      if (is.null(bv)) bv <- NA
      if (is.null(cv)) cv <- NA
      # Numeric tolerance: a tiny float diff isn't a real change
      if (is.numeric(bv) && is.numeric(cv) && !is.na(bv) && !is.na(cv)) {
        if (abs(bv - cv) > 1e-9) changed[[k]] <- list(from = bv, to = cv)
      } else if (!identical(bv, cv)) {
        changed[[k]] <- list(from = bv, to = cv)
      }
    }
    jsonlite::toJSON(changed, auto_unbox = TRUE, na = "null")
  }, error = function(e) "{}")
}

# ---------------------------------------------------------------------------
# Log a results-view event.
#
# session_id           : same hash used for session_start
# ip / user_country    : repeated per row so the events table is queryable
#                        without joining
# model_country        : input$region at time of view
# scenario1_inputs     : named list of scenario1 intervention values
# scenario2_inputs     : named list of scenario2 intervention values
# baseline_inputs      : named list of baseline intervention values
# session_baseline_snapshot : the snapshot taken on FIRST results-view of
#                             this session; pass NULL on first view
# view_count           : 1, 2, 3, ... (caller maintains the counter)
# ---------------------------------------------------------------------------
log_results_view <- function(session_id,
                             ip = NA_character_,
                             user_country = "unknown",
                             model_country = NA_character_,
                             scenario1_inputs = list(),
                             scenario2_inputs = list(),
                             baseline_inputs = list(),
                             session_baseline_snapshot = NULL,
                             view_count = 1L) {
  
  # delta_json is no longer computed at log time -- the original
  # "first-results-view snapshot" design proved misleading (first row
  # always logged empty, only captured between-view changes).
  #
  # Analysis-time code now derives the meaningful delta directly from
  # scenario1_json vs baseline_json on each row. We write NA to keep
  # the schema stable for existing DBs; the column may be dropped in a
  # future migration.
  #
  # session_baseline_snapshot is kept as a parameter for backward
  # compatibility with callers that still pass it; it is no longer used.
  
  .write_event(list(
    timestamp      = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    session_id     = session_id,
    ip_hash        = hash_ip(ip),
    user_country   = user_country,
    event_type     = "results_view",
    model_country  = model_country %||% NA_character_,
    scenario1_json = tryCatch(jsonlite::toJSON(scenario1_inputs, auto_unbox = TRUE, na = "null"),
                              error = function(e) "{}"),
    scenario2_json = tryCatch(jsonlite::toJSON(scenario2_inputs, auto_unbox = TRUE, na = "null"),
                              error = function(e) "{}"),
    baseline_json  = tryCatch(jsonlite::toJSON(baseline_inputs, auto_unbox = TRUE, na = "null"),
                              error = function(e) "{}"),
    delta_json     = NA_character_,
    view_count     = as.integer(view_count)
  ))
}

# ---------------------------------------------------------------------------
# Read helper -- used by ad-hoc analysis scripts AND (later) the admin
# dashboard. Keeping this in the logger module means dashboard code reuses
# the same read path.
#
# Returns a data.frame of all rows, or an empty data.frame if the DB doesn't
# exist yet.
# ---------------------------------------------------------------------------
read_usage_db <- function(path = .TIER_LOG_PATH) {
  if (is.null(path) || !file.exists(path)) {
    return(data.frame())
  }
  tryCatch({
    con <- dbConnect(SQLite(), path)
    on.exit(dbDisconnect(con), add = TRUE)
    dbGetQuery(con, "SELECT * FROM events ORDER BY event_id")
  }, error = function(e) data.frame())
}