COUCHDB_TEST_USER <- Sys.getenv("COUCHDB_TEST_USER")
COUCHDB_TEST_PWD <- Sys.getenv("COUCHDB_TEST_PWD")

context("couchdb: src")
test_that("Source", {
  skip_on_cran()

  skip_if_no_couchdb()
  src <- src_couchdb(user=COUCHDB_TEST_USER, pwd=COUCHDB_TEST_PWD)
  expect_is(src, "docdb_src")
  expect_is(src, "src_couchdb")
  expect_is(unclass(src), "list")
  expect_equal(attr(src, "type"), "couchdb")
  expect_equal(attr(src, "info")$couchdb, "Welcome")
})

context("couchdb: create")
test_that("db into couchdb", {
  skip_on_cran()

  df <- data.frame(a = letters[1:10], b = LETTERS[1:10], stringsAsFactors = FALSE)

  skip_if_no_couchdb()
  src <- src_couchdb(user=COUCHDB_TEST_USER, pwd=COUCHDB_TEST_PWD)

  # delete if exists
  invisible(tryCatch(docdb_delete(src, "df"), error = function(e) e))

  invisible(docdb_create(src, "df", df))
  d2 <- docdb_get(src, "df")
  expect_equal(data.frame(d2), df)
})

context("couchdb: delete")
test_that("delete in couchdb works", {
  skip_on_cran()
  
  df <- data.frame(a = letters[1:10], b = LETTERS[1:10], stringsAsFactors = FALSE)
  
  skip_if_no_couchdb()
  src <- src_couchdb(user=COUCHDB_TEST_USER, pwd=COUCHDB_TEST_PWD)

  # delete if exists
  invisible(tryCatch(docdb_delete(src, "foobar"), error = function(e) e))

  invisible(docdb_create(src, "foobar", df))
  del <- docdb_delete(src, "foobar")
  expect_true(del$ok)
})


context("couchdb: exists")
test_that("exists in couchdb works", {
  skip_on_cran()
  
  df <- data.frame(a = letters[1:10], b = LETTERS[1:10], stringsAsFactors = FALSE)
  
  skip_if_no_couchdb()
  src <- src_couchdb(user=COUCHDB_TEST_USER, pwd=COUCHDB_TEST_PWD)

  # delete if exists
  invisible(tryCatch(docdb_delete(src, "foobar"), error = function(e) e))

  expect_false(docdb_exists(src, "foobar"))

  invisible(docdb_create(src, "foobar", df))
  
  expect_true(docdb_exists(src, "foobar"))
})


context("couchdb: query")
test_that("query in couchdb works", {
  skip_on_cran()
  skip_if_no_couchdb()
  x <- sofa::Cushion$new()$ping()
  couch_ver <- as.numeric(substring(gsub("\\.", "", x$version), 1, 1))
  skip_if(couch_ver < 2)

  src <- src_couchdb(user=COUCHDB_TEST_USER, pwd=COUCHDB_TEST_PWD)

  if (docdb_exists(src, "mtcars2")) docdb_delete(src, "mtcars2")
  invisible(docdb_create(src, key = "mtcars2", value = mtcars))
  expect_true(docdb_exists(src, "mtcars2"))
  query <- list(cyl = list("$gt" = 6))
  res <- docdb_query(src, "mtcars2", query = query)
  expect_is(res, "data.frame")
  expect_equal(NROW(res), 10)
})


