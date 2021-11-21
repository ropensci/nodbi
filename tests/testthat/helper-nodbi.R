## helper for tests --------------------------------------

createKey <- function() paste0(
  "test_nodbi_",
  format(Sys.time(),
         "%Y-%m-%d_%H-%M-%S",
         tz = "UTC"), "_",
  paste0(sample(letters, 4), collapse = "")
)

skip_if_no_couchdb <- function() {
  testthat::skip_if_not_installed("sofa")
  COUCHDB_TEST_USER <- Sys.getenv("COUCHDB_TEST_USER")
  COUCHDB_TEST_PWD <- Sys.getenv("COUCHDB_TEST_PWD")
  if (inherits(try(
    src_couchdb(user = COUCHDB_TEST_USER, pwd = COUCHDB_TEST_PWD),
    silent = TRUE), "try-error")) {
    skip("couchdb is not available")
  }
}

skip_if_no_mongo <- function() {
  testthat::skip_if_not_installed("mongolite")
  if (inherits(try(src_mongo(), silent = TRUE), "try-error")) {
    skip("mongodb is not available")
  }
}

skip_if_no_sqlite <- function() {
  testthat::skip_if_not_installed("RSQLite")
  if (inherits(try(tmp <- src_sqlite(), silent = TRUE), "try-error")) {
    skip("sqlite is not available")
  }
  RSQLite::dbDisconnect(tmp$con)
}

skip_if_no_elastic <- function() {
  testthat::skip_if_not_installed("elastic")
  if (inherits(try(src_elastic(), silent = TRUE), "try-error")) {
    skip("elasticsearch is not available")
  }
}
