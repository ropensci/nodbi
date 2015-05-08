context("etcd")

src <- src_etcd()

test_that("Source", {
  expect_is(src, "docdb_src")
  expect_is(src, "src_etcd")
  expect_is(unclass(src), "list")
  expect_is(src$version, "character")
})

df <- data.frame(a = letters[1:10], b = LETTERS[1:10], stringsAsFactors = FALSE)

test_that("db into etcd", {
  # delete if exists
  invisible(tryCatch(docdb_delete(src, "/df"), error = function(e) e))

  docdb_create(src, "/df", df)
  d2 <- docdb_get(src, "/df")
  expect_equal(data.frame(d2), df)
})

test_that("delete in etcd works", {
  # delete if exists
  invisible(tryCatch(docdb_delete(src, "/df"), error = function(e) e))

  docdb_create(src, "/df", df)
  del <- docdb_delete(src, "/df")
  expect_equal(del$action, "delete")
})
