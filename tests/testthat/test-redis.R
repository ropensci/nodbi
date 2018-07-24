context("redis")

test_that("Source (Redis)", {
  skip_on_cran()

  skip_if_no_redis()
  src <- src_redis()
  expect_that(src, is_a("docdb_src"))
  expect_that(src, is_a("src_redis"))
  expect_that(src$con, is_a("redis_api"))
  expect_that(src$type, equals("redis"))
  expect_equal(src$con$type(), "redux")
})

test_that("db into Redis", {
  skip_on_cran()
  
  skip_if_no_redis()
  d <- mtcars
  key <- "mtcars"
  con <- src_redis()
  docdb_create(con, key, d)
  d2 <- docdb_get(con, key)
  expect_that(d2, equals(d))
})
