context("couchdb")


test_that("Source", {
  skip_if_no_couchdb()
  src <- src_couchdb()
  expect_is(src, "docdb_src")
  expect_is(src, "src_couchdb")
  expect_is(unclass(src), "list")
  expect_equal(src$couchdb, "Welcome")
})

df <- data.frame(a = letters[1:10], b = LETTERS[1:10], stringsAsFactors = FALSE)

test_that("db into couchdb", {
  skip_if_no_couchdb()
  src <- src_couchdb()

  # delete if exists
  invisible(tryCatch(docdb_delete(src, "df"), error = function(e) e))

  invisible(docdb_create(src, "df", df))
  d2 <- docdb_get(src, "df")
  expect_equal(data.frame(d2), df)
})

test_that("delete in couchdb works", {
  skip_if_no_couchdb()
  src <- src_couchdb()

  # delete if exists
  invisible(tryCatch(docdb_delete(src, "df"), error = function(e) e))

  invisible(docdb_create(src, "df", df))
  del <- docdb_delete(src, "df")
  expect_true(del$ok)
})
