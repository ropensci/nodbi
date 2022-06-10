
#### params for tests ####

# delay needed
elasticSleep <- 1L # seconds

# Warning (core-nodbi.R:176:3): docdb_update, docdb_query
# Changing language has no effect when envvar LANG='C'
Sys.unsetenv("LANG")



#### data for tests ####

testDf <- mtcars # has rownames
testDf2 <- iris  # no rownames
# factors cannot be expected to be maintained
testDf2[["Species"]] <- as.character(testDf2[["Species"]])

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

testUrl <- "http://httpbin.org/stream/98"



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

skip_if_no_postgres <- function() {
  testthat::skip_if_not_installed("RPostgres")
  if (inherits(try(tmp <- src_postgres(), silent = TRUE), "try-error")) {
    skip("postgres is not available")
  }
  RPostgres::dbDisconnect(tmp$con)
}

