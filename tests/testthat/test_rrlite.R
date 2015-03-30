context("rrlite")

test_that("Source", {
  src <- src_rrlite()
  expect_that(src, is_a("docdb_src"))
  expect_that(src, is_a("src_rrlite"))
  expect_that(src$con, is_a("hiredis"))
  expect_that(src$con$context, is_a("rlite_context"))
})

test_that("db into redis", {
  d <- mtcars
  key <- "mtcars"
  con <- src_rrlite()
  docdb_create(con, key, d)
  d2 <- docdb_get(con, key)
  expect_that(d2, equals(d))
})
