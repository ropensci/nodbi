context("etcd: src")
test_that("Source", {
  skip_on_cran()

  skip_if_no_etcd()
  src <- src_etcd()
  expect_is(src, "docdb_src")
  expect_is(src, "src_etcd")
  expect_is(attr(src, "version"), "list")
  expect_is(attr(src, "version")$etcdserver, "character")
})

df <- data.frame(a = letters[1:10], b = LETTERS[1:10], stringsAsFactors = FALSE)

context("etcd: create")
test_that("db into etcd", {
  skip_on_cran()

  skip_if_no_etcd()
  src <- src_etcd()

  # delete if exists
  invisible(tryCatch(suppressWarnings(docdb_delete(src, "/ddd")), error = function(e) e))

  docdb_create(src, "/ddd", df)
  d2 <- docdb_get(src, "/ddd")
  expect_equal(data.frame(d2), df)
})

context("etcd: delete")
test_that("delete in etcd works", {
  skip_on_cran()
  
  skip_if_no_etcd()
  src <- src_etcd()

  # delete if exists
  invisible(tryCatch(suppressWarnings(docdb_delete(src, "/df")), error = function(e) e))

  docdb_create(src, "/df", df)
  del <- docdb_delete(src, "/df")
  expect_equal(del$action, "delete")
})
