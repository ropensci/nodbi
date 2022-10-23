test_that("Source", {
  skip_on_cran()
  skip_if_no_duckdb()

  # creating database connection has
  # unique parameters, see README.Rmd
  dbSrcKey <- function() {
    testKey <- createKey()
    testSrc <- src_duckdb()
    return(list(testKey = testKey, testSrc = testSrc))
    }

  # test database set up
  tmp <- dbSrcKey()
  expect_is(tmp$testSrc, "docdb_src")
  expect_is(tmp$testSrc, "src_duckdb")
  expect_output(print(tmp$testSrc), "duckdb")
  duckdb::dbDisconnect(tmp$testSrc$con, shutdown = TRUE)
  rm(tmp)

  # run generic (database independent) tests
  source("core-nodbi.R", local = TRUE)
})
