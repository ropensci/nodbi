context("mongodb")

test_that("Source", {
  src <- src_mongo()
  expect_is(src, "docdb_src")
  expect_is(src, "src_mongo")
  expect_that(src$con, is_a("mongo_client"))
  expect_equal(src$db, "test")
})

test_that("db into mongo", {
  con <- src_mongo()
  # delete if exists
  invisible(tryCatch(docdb_delete(con, "iris"), error = function(e) e))

  iris$Species <- as.character(iris$Species)
  docdb_create(con, "iris", iris)
  d2 <- docdb_get(con, "iris")
  expect_equal(d2, iris)
})

test_that("delete in mongo works", {
  cnn <- src_mongo()
  # delete if exists
  invisible(tryCatch(docdb_delete(cnn, "iris"), error = function(e) e))

  docdb_create(cnn, "flowers", iris)
  del <- docdb_delete(cnn, "flowers")
  expect_true(del)
})
