
#### params for tests ####

# Warning (core-nodbi.R:176:3): docdb_update, docdb_query
# Changing language has no effect when envvar LANG='C'
Sys.unsetenv("LANG")

# test verbose messages
# options(verbose = TRUE)

# for interactive testing:
# devtools::load_all()
# source("tests/testthat/helper.R")
# go to "tests/testthat/test-*.R"
# go to "tests/testthat/core-nodbi.R"

#### data for tests ####

testDf <- mtcars # has rownames
testDf2 <- iris  # no rownames
# factors cannot be expected to be maintained
testDf2[["Species"]] <- as.character(testDf2[["Species"]])

ndjson <- NULL
#
jsonlite::stream_out(
  jsonlite::fromJSON(contacts),
  con = textConnection("ndjson", open = "w", local = TRUE),
  verbose = FALSE, auto_unbox = TRUE)
#
testDf3 <- data.frame(
  `json` = ndjson,
  stringsAsFactors = FALSE)
#
testDf4 <- data.frame(
  `_id` = uuid::UUIDgenerate(n = nrow(testDf3)),
  `json` = ndjson,
  stringsAsFactors = FALSE,
  check.names = FALSE)

testJson <- contacts # has _id's
testJson2 <- mapdata # no _id's

testList <- jsonlite::fromJSON(mapdata, simplifyVector = FALSE)

testFile <- function(..., env = parent.frame()) {
  testFile <- tempfile(fileext = ".ndjson")
  jsonlite::stream_out(jsonlite::fromJSON(contacts), con = file(testFile), verbose = FALSE)
  withr::defer(
    try(unlink(testFile), silent = TRUE),
    envir = env)
  testFile
}

testFile2 <- function(..., env = parent.frame()) {
  testFile2 <- tempfile(fileext = ".ndjson")
  jsonlite::stream_out(diamonds, con = file(testFile2), verbose = FALSE)
  withr::defer(
    try(unlink(testFile2), silent = TRUE),
    envir = env)
  testFile2
}

if (require("webfakes", quietly = TRUE) &&
    packageVersion("webfakes") >= package_version("1.2.0")) {
  app <- webfakes::new_app()
  httpbin <- webfakes::local_app_process(webfakes::httpbin_app())
  httpbin$start()
} else {
  httpbin <- NULL
}

#### helper for tests ####

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
  if (inherits(try(
    RSQLite::dbDisconnect(suppressWarnings(src_sqlite()$con)),
    silent = TRUE), "try-error")) {
    skip("sqlite is not available")
  }
}

skip_if_no_elastic <- function() {
  testthat::skip_if_not_installed("elastic")
  if (inherits(try(src_elastic(), silent = TRUE), "try-error")) {
    skip("elasticsearch is not available")
  }
}

skip_if_no_postgres <- function() {
  testthat::skip_if_not_installed("RPostgres")
  if (inherits(try(
    RPostgres::dbDisconnect(src_postgres()$con, shutdown = TRUE),
    silent = TRUE), "try-error")) {
    skip("postgres is not available")
  }
}

skip_if_no_duckdb <- function() {
  testthat::skip_if_not_installed("duckdb")
  if (inherits(try(
    duckdb::dbDisconnect(suppressWarnings(src_duckdb()$con), shutdown = TRUE),
    silent = TRUE), "try-error")) {
    skip("duckdb or its JSON extension is not available")
  }
}
