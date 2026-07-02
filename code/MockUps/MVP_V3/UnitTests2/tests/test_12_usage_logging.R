# ============================================================================
# test_12_usage_logging.R
# ----------------------------------------------------------------------------
# Tests for the usage_logger.R module.
#
# The logger is a pure side-effect module: its job is to write rows to a
# SQLite database. Tests verify that:
#
#   12.1  init_log_db creates the events table at a writable path
#   12.2  hash_ip is deterministic for the same IP, different for different
#         IPs, and salted (different across init_log_db calls)
#   12.3  log_session_start writes one row with expected fields
#   12.4  log_results_view writes one row with expected fields and JSON
#         payloads parse back to the original lists
#   12.5  compute_delta correctly identifies changed fields and ignores
#         unchanged ones (including tiny float differences)
#   12.6  Logger fails silently when the DB path is unwritable -- the calling
#         code must NEVER crash
#   12.7  extract_client_ip prefers X-Forwarded-For over REMOTE_ADDR
#   12.8  resolve_country returns "local" for private IPs and "unknown" for
#         clearly bad inputs (does NOT hit the network in tests)
#
# These tests are purely additive. No existing test file is affected.
# ============================================================================

library(testthat)

# Source the logger from the same directory as this test file.
source("usage_logger.R")

# ---------------------------------------------------------------------------
# Helper: fresh temp DB for each test, cleaned up after.
# ---------------------------------------------------------------------------
fresh_db <- function() {
  p <- tempfile(fileext = ".sqlite")
  init_log_db(candidate_paths = p)
  p
}

# ---------------------------------------------------------------------------
# 12.1 init_log_db creates events table
# ---------------------------------------------------------------------------
# WHAT: After init_log_db, the SQLite file contains an `events` table with
#       the expected columns.
# WHY:  All subsequent writes depend on this schema. Pinning column names
#       here means future schema drift will break this test before it
#       silently breaks the analytics.
# HOW:  Init with a tempfile, open with DBI, check column list.
# ---------------------------------------------------------------------------
test_that("init_log_db creates events table with expected columns", {
  p <- fresh_db()
  con <- dbConnect(SQLite(), p)
  on.exit(dbDisconnect(con))
  
  cols <- dbGetQuery(con, "PRAGMA table_info(events)")$name
  expected <- c("event_id", "timestamp", "session_id", "ip_hash",
                "user_country", "event_type", "model_country",
                "scenario1_json", "scenario2_json", "baseline_json",
                "delta_json", "view_count")
  expect_setequal(cols, expected)
})

# ---------------------------------------------------------------------------
# 12.2 hash_ip is deterministic, distinguishing, and salted
# ---------------------------------------------------------------------------
# WHAT: Same IP -> same hash within one server session. Different IPs ->
#       different hashes. After a fresh init_log_db (= new salt), the
#       same IP produces a DIFFERENT hash.
# WHY:  Determinism is needed to count unique visitors within a session.
#       Salt rotation is the privacy guarantee -- restarting the server
#       severs the cross-session link.
# HOW:  Init, hash twice, compare. Re-init, hash same IP, compare different.
# ---------------------------------------------------------------------------
test_that("hash_ip is deterministic within session and distinguishes IPs", {
  fresh_db()
  h1 <- hash_ip("8.8.8.8")
  h2 <- hash_ip("8.8.8.8")
  h3 <- hash_ip("1.1.1.1")
  expect_equal(h1, h2)
  expect_false(identical(h1, h3))
  expect_true(nchar(h1) == 64)  # sha256 hex length
})

test_that("hash_ip changes when salt rotates (server restart simulation)", {
  fresh_db()
  h1 <- hash_ip("8.8.8.8")
  fresh_db()   # new salt
  h2 <- hash_ip("8.8.8.8")
  expect_false(identical(h1, h2))
})

test_that("hash_ip returns NA for empty/null input", {
  fresh_db()
  expect_true(is.na(hash_ip(NULL)))
  expect_true(is.na(hash_ip("")))
  expect_true(is.na(hash_ip(NA)))
})

# ---------------------------------------------------------------------------
# 12.3 log_session_start writes one row with expected fields
# ---------------------------------------------------------------------------
# WHAT: After log_session_start, the events table has exactly one row with
#       event_type = "session_start", the right session_id, and NULL
#       scenario fields.
# WHY:  Smoke test that the write path works end-to-end. If this fails,
#       nothing else in the logger works either.
# HOW:  Init DB, call log_session_start, query back, assert.
# ---------------------------------------------------------------------------
test_that("log_session_start writes a single row with expected fields", {
  p <- fresh_db()
  log_session_start(session_id = "sess_abc",
                    ip = "8.8.8.8",
                    user_country = "United States")
  
  rows <- read_usage_db(p)
  expect_equal(nrow(rows), 1)
  expect_equal(rows$event_type, "session_start")
  expect_equal(rows$session_id, "sess_abc")
  expect_equal(rows$user_country, "United States")
  expect_true(!is.na(rows$ip_hash))
  expect_true(is.na(rows$model_country))
  expect_true(is.na(rows$scenario1_json))
})

# ---------------------------------------------------------------------------
# 12.4 log_results_view writes one row with parseable JSON payloads
# ---------------------------------------------------------------------------
# WHAT: After log_results_view, the row contains JSON that round-trips back
#       to the original input lists.
# WHY:  The whole point of this row is the input snapshot. If JSON encoding
#       silently mangles values, downstream analysis would be wrong without
#       any visible error.
# HOW:  Build small intervention lists, log, read back, parse JSON, compare.
# ---------------------------------------------------------------------------
test_that("log_results_view writes a row with parseable JSON", {
  p <- fresh_db()
  s1 <- list(prep_oral = 1000, vmmc = 5000, mmd_3month = 25)
  s2 <- list(prep_oral = 2000, vmmc = 5000, mmd_3month = 50)
  base <- list(prep_oral = 800, vmmc = 4000, mmd_3month = 20)
  
  log_results_view(session_id = "sess_xyz",
                   ip = "8.8.8.8",
                   user_country = "United States",
                   model_country = "Zambia",
                   scenario1_inputs = s1,
                   scenario2_inputs = s2,
                   baseline_inputs = base,
                   session_baseline_snapshot = NULL,
                   view_count = 1L)
  
  rows <- read_usage_db(p)
  expect_equal(nrow(rows), 1)
  expect_equal(rows$event_type, "results_view")
  expect_equal(rows$model_country, "Zambia")
  expect_equal(rows$view_count, 1L)
  
  s1_back <- jsonlite::fromJSON(rows$scenario1_json)
  expect_equal(s1_back$prep_oral, 1000)
  expect_equal(s1_back$vmmc, 5000)
  expect_equal(s1_back$mmd_3month, 25)
  
  base_back <- jsonlite::fromJSON(rows$baseline_json)
  expect_equal(base_back$prep_oral, 800)
})

# ---------------------------------------------------------------------------
# 12.5 compute_delta identifies changes correctly
# ---------------------------------------------------------------------------
# WHAT: Changed fields appear in the delta JSON; unchanged fields and
#       sub-tolerance float differences do not.
# WHY:  The delta is what answers "which inputs do users actually adapt".
#       False positives (logging unchanged fields) inflate the signal.
# HOW:  Three cases: real change (1000 -> 2000), no change, tiny float diff.
# ---------------------------------------------------------------------------
test_that("compute_delta captures real changes and ignores noise", {
  baseline <- list(a = 1000, b = 50, c = 0.5)
  current  <- list(a = 2000, b = 50, c = 0.5 + 1e-12)  # only `a` changed
  
  delta_json <- compute_delta(current, baseline)
  delta <- jsonlite::fromJSON(delta_json, simplifyVector = FALSE)
  
  expect_true("a" %in% names(delta))
  expect_false("b" %in% names(delta))
  expect_false("c" %in% names(delta))   # float diff below tolerance
  expect_equal(delta$a$from, 1000)
  expect_equal(delta$a$to, 2000)
})

test_that("compute_delta returns empty JSON when baseline is NULL", {
  result <- compute_delta(list(a = 1, b = 2), NULL)
  expect_equal(result, "{}")
})

# ---------------------------------------------------------------------------
# 12.6 Logger fails silently when DB is unwritable
# ---------------------------------------------------------------------------
# WHAT: If init_log_db cannot find any writable path, subsequent log_* calls
#       must not throw errors -- they must silently no-op.
# WHY:  THE most important property of the logger. A logging bug must
#       never crash the user's session. This is the regression guard.
# HOW:  Provide a single unwritable candidate path. Confirm init returns
#       NULL and log_* calls do not throw.
# ---------------------------------------------------------------------------
test_that("logger no-ops silently when init fails", {
  # An impossible path on every system
  result <- suppressWarnings(
    init_log_db(candidate_paths = "/nonexistent_root_dir_xyz/usage.sqlite")
  )
  expect_null(result)
  
  # These must not throw
  expect_silent(log_session_start("s1", "8.8.8.8", "US"))
  expect_silent(log_results_view("s1",
                                 ip = "8.8.8.8",
                                 user_country = "US",
                                 model_country = "Zambia",
                                 scenario1_inputs = list(a = 1),
                                 scenario2_inputs = list(a = 2),
                                 baseline_inputs = list(a = 0),
                                 session_baseline_snapshot = NULL,
                                 view_count = 1L))
})

# ---------------------------------------------------------------------------
# 12.7 extract_client_ip prefers X-Forwarded-For
# ---------------------------------------------------------------------------
# WHAT: When session$request has both HTTP_X_FORWARDED_FOR and REMOTE_ADDR,
#       the X-F-F value wins. When only REMOTE_ADDR is present, it falls
#       back. Comma-separated X-F-F chains return the first IP.
# WHY:  Shiny-server commonly sits behind nginx/Apache. Without X-F-F
#       handling, every user IP would be 127.0.0.1.
# HOW:  Build a fake session object with the relevant fields.
# ---------------------------------------------------------------------------
test_that("extract_client_ip prefers X-Forwarded-For", {
  fake_session_xff <- list(
    request = list(
      HTTP_X_FORWARDED_FOR = "203.0.113.5",
      REMOTE_ADDR = "127.0.0.1"
    )
  )
  expect_equal(extract_client_ip(fake_session_xff), "203.0.113.5")
  
  fake_session_chain <- list(
    request = list(
      HTTP_X_FORWARDED_FOR = "203.0.113.5, 10.0.0.1, 192.168.1.1",
      REMOTE_ADDR = "127.0.0.1"
    )
  )
  expect_equal(extract_client_ip(fake_session_chain), "203.0.113.5")
  
  fake_session_direct <- list(
    request = list(
      REMOTE_ADDR = "203.0.113.42"
    )
  )
  expect_equal(extract_client_ip(fake_session_direct), "203.0.113.42")
})

# ---------------------------------------------------------------------------
# 12.8 resolve_country handles private/loopback IPs without network calls
# ---------------------------------------------------------------------------
# WHAT: resolve_country returns "local" for any RFC1918 / loopback range
#       and "unknown" for empty input. These cases MUST NOT issue HTTP
#       requests.
# WHY:  In dev / behind misconfigured proxies, users could appear with
#       private IPs. We should detect this without slamming ipapi.co with
#       guaranteed-failing requests.
# HOW:  Verify the function returns the expected sentinel strings. We do
#       NOT test the network-success path here -- that requires mocking
#       httr and would be flaky.
# ---------------------------------------------------------------------------
test_that("resolve_country handles private and empty IPs without network", {
  expect_equal(resolve_country("127.0.0.1"), "local")
  expect_equal(resolve_country("10.0.0.5"), "local")
  expect_equal(resolve_country("192.168.1.100"), "local")
  expect_equal(resolve_country("172.16.0.1"), "local")
  expect_equal(resolve_country(""), "unknown")
  expect_equal(resolve_country(NA), "unknown")
  expect_equal(resolve_country(NULL), "unknown")
})
