skip_if_no_redis <- function() {
  testthat::skip_if_not_installed("redux")
  if (redux::redis_available()) {
    return()
  }
  skip("redis is not available")
}

skip_if_no_couchdb <- function() {
  testthat::skip_if_not_installed("sofa")
  if (inherits(try(src_couchdb(), silent = TRUE), "try-error")) {
    skip("couchdb is not available")
  }
}

skip_if_no_mongo <- function() {
  testthat::skip_if_not_installed("mongolite")
  if (inherits(try(src_mongo(), silent=TRUE), "try-error")) {
    skip("mongodb is not available")
  }
}

skip_if_no_sqlite <- function() {
  testthat::skip_if_not_installed("RSQLite")
  if (inherits(try(src_sqlite(), silent = TRUE), "try-error")) {
    skip("sqlite is not available")
  }
}
