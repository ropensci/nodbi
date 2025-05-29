test_that("Source", {
  skip_on_cran()
  skip_if_no_sqlite()

  # creating database connection has
  # unique parameters, see README.Rmd
  dbSrcKey <- function() {
    testKey <- createKey()
    testSrc <- suppressWarnings(src_sqlite())
    return(list(testKey = testKey, testSrc = testSrc))
  }

  # test database set up
  tmp <- dbSrcKey()
  expect_is(tmp$testSrc, "docdb_src")
  expect_is(tmp$testSrc, "src_sqlite")
  expect_output(print(tmp$testSrc), "src: SQLite")
  RSQLite::dbDisconnect(tmp$testSrc$con)
  rm(tmp)

  # run generic (database independent) tests
  source("core-nodbi.R", local = TRUE)

})
