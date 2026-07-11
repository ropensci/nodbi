test_that("Source", {
  skip_if_no_mariadb()
  skip_on_cran()

  # creating database connection has
  # unique parameters, see README.Rmd
  dbSrcKey <- function() {
    testKey <- createKey()
    testSrc <- src_mariadb()
    return(list(testKey = testKey, testSrc = testSrc))
  }

  # test database set up
  tmp <- dbSrcKey()
  expect_is(tmp$testSrc, "docdb_src")
  expect_is(tmp$testSrc, "src_mariadb")
  expect_output(suppressWarnings(print(tmp$testSrc)), "src: MariaDB")
  RMariaDB::dbDisconnect(tmp$testSrc$con)
  rm(tmp)

  # run generic (database independent) tests
  source("core-nodbi.R", local = TRUE)

})
