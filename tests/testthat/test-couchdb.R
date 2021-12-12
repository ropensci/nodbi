test_that("Source", {
  skip_on_cran()
  skip_if_no_couchdb()

  COUCHDB_TEST_USER <- Sys.getenv("COUCHDB_TEST_USER")
  COUCHDB_TEST_PWD <- Sys.getenv("COUCHDB_TEST_PWD")

  # creating database connection has
  # unique parameters, see README.Rmd
  dbSrcKey <- function() {
    testKey <- createKey()
    testSrc <- src_couchdb(user = COUCHDB_TEST_USER, pwd = COUCHDB_TEST_PWD)
    return(list(testKey = testKey, testSrc = testSrc))
  }

  # test database set up
  tmp <- dbSrcKey()
  expect_is(tmp$testSrc, "docdb_src")
  expect_is(tmp$testSrc, "src_couchdb")
  expect_equal(attr(tmp$testSrc, "type"), "couchdb")
  expect_equal(attr(tmp$testSrc, "info")$couchdb, "Welcome")
  expect_output(print(tmp$testSrc), "src: couchdb")
  rm(tmp)

  # run generic (database independent) tests
  source("core-nodbi.R", local = TRUE)

})
