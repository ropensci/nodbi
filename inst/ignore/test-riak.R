context("riak")


test_that("Source", {
  skip_if_no_riak()
  src <- src_riak()

  expect_is(src, "docdb_src")
  expect_is(src, "src_riak")
  expect_is(attr(src, "info"), "list")
  expect_is(attr(src, "info")$riak_kv_version, "character")
})

df <- data.frame(a = letters[1:10], b = LETTERS[1:10], stringsAsFactors = FALSE)

test_that("db into riak", {
  skip_if_no_riak()
  src <- src_riak()

  # delete if exists
  invisible(tryCatch(suppressWarnings(docdb_delete(src, "ddd")), error = function(e) e))

  docdb_create(src, "ddd", df)
  d2 <- docdb_get(src, "ddd")
  expect_equal(data.frame(d2), df)
})

test_that("delete in riak works", {
  skip_if_no_riak()
  src <- src_riak()

  # delete if exists
  invisible(tryCatch(suppressWarnings(docdb_delete(src, "df")), error = function(e) e))

  docdb_create(src, "df", df)
  del <- docdb_delete(src, "df")
  expect_true(del)
})
