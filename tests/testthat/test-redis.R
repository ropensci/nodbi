context("redis")

test_that("Source (Redis)", {
  skip_if_no_redis()
  src <- src_redis()
  expect_that(src, is_a("docdb_src"))
  expect_that(src, is_a("src_redis"))
  expect_that(src$con, is_a("redis_api"))
  expect_that(src$type, equals("redis"))
  expect_equal(src$con$type(), "RcppRedis")
})

test_that("Source (rlite)", {
  skip_if_no_rrlite()
  src <- src_rlite()
  expect_that(src, is_a("docdb_src"))
  expect_that(src, is_a("src_rlite"))
  expect_that(src, is_a("src_redis"))
  expect_that(src$con, is_a("redis_api"))
  expect_that(src$type, equals("redis"))
  expect_equal(src$con$type(), "rrlite")
})

test_that("db into Redis", {
  skip_if_no_redis()
  d <- mtcars
  key <- "mtcars"
  con <- src_redis()
  docdb_create(con, key, d)
  d2 <- docdb_get(con, key)
  expect_that(d2, equals(d))
})

## Hmm, not sorking here...
test_that("db into rlite", {
  skip_if_no_rrlite()
  d <- mtcars
  key <- "mtcars"
  con <- src_rlite()
  docdb_create(con, key, d)
  d2 <- docdb_get(con, key)
  expect_that(d2, equals(d))
})
