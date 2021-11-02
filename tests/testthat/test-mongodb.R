test_that("Source", {
  skip_on_cran()
  skip_if_no_mongo()

  # creating database connection has
  # unique parameters, see README.Rmd
  dbSrcKey <- function() {
    testKey <- createKey()
    testSrc <- src_mongo(collection = testKey)
    return(list(testKey = testKey, testSrc = testSrc))
    }

  # test database set up
  tmp <- dbSrcKey()
  expect_is(tmp$testSrc, "docdb_src")
  expect_is(tmp$testSrc, "src_mongo")
  expect_output(print(tmp$testSrc), "MongoDB")
  rm(tmp)

  # run generic (database independent) tests
  source("core-nodbi.R", local = TRUE)
})
