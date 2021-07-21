context("mongodb: src")
test_that("Source", {
  skip_on_cran()

  skip_if_no_mongo()
  src <- src_mongo()
  expect_is(src, "docdb_src")
  expect_is(src, "src_mongo")
  expect_is(src$con, "mongo")
  expect_equal(src$db, "test")

  capture.output(print(src))
})

context("mongodb: create")
test_that("db into mongo", {
  skip_on_cran()

  skip_if_no_mongo()
  con <- src_mongo(collection = "iris")
  # delete if exists
  invisible(tryCatch(docdb_delete(con, "iris"), error = function(e) e))

  iris$Species <- as.character(iris$Species)

  expect_message(
    docdb_create(con, "notiris", iris),
    "Parameter 'key' is different from parameter 'collection'")

  expect_message(
    docdb_get(con, "notiris"),
    "Parameter 'key' is different from parameter 'collection'")
  d2 <- docdb_get(con, "iris")

  # FIXME: skipping for now, apparently just the names of data.frame
  # are different; mongolite seems to replace dots with underscores
  names(d2) <- NULL
  names(iris) <- NULL
  expect_equal(d2, iris)

  # create json
  docdb_create(
    con, "iris",
    data.frame(
      "_id" = "someid",
      "somejson" = contacts,
      check.names = FALSE,
      stringsAsFactors = FALSE
    ))
  d2 <- docdb_get(con, "iris")
  expect_true(mean(d2$age, na.rm = TRUE) == 23L)

  # create json
  docdb_create(
    con, "iris",
    data.frame(
      "_id" = "someid",
      "otherjson" = mapdata,
      check.names = FALSE,
      stringsAsFactors = FALSE
    ))
  d2 <- docdb_get(con, "iris")
  # TODO here but not with src_sqlite we have a double [[1]]
  d2 <- d2[!sapply(d2$rows, is.null), ]
  d2 <- d2$rows[[1]][[1]]$elements[[1]]$duration$somevalue
  expect_identical(d2, c(14064L, 151772L, 67405L, 152913L))

  # delete if exists
  invisible(tryCatch(docdb_delete(con, "iris"), error = function(e) e))

})

context("mongodb: delete")
test_that("delete in mongo works", {
  skip_on_cran()

  skip_if_no_mongo()
  con <- src_mongo(collection = "iris")

  # delete if exists
  expect_message(
    docdb_delete(con, "notiris"),
    "Parameter 'key' is different from parameter 'collection'")
  invisible(tryCatch(docdb_delete(con, "iris"), error = function(e) e))

  docdb_create(con, "iris", iris)
  del <- docdb_delete(con, "iris", query = '{"Species": "setosa"}')
  expect_true(del)
  d2 <- docdb_get(con, "iris")
  expect_equal(nrow(d2), 100L)
  # clean up
  invisible(tryCatch(docdb_delete(con, "iris"), error = function(e) e))

  docdb_create(con, "iris", iris)
  del <- docdb_delete(con, "iris")
  expect_true(del)
})

context("mongodb: exists")
test_that("exists in mongo works", {
  skip_on_cran()

  skip_if_no_mongo()
  con <- src_mongo(collection = "doesnotexist")
  expect_false(docdb_exists(con, "doesnotexist2"))

  docdb_create(con, "doesnotexist", iris)
  expect_true(docdb_exists(con, key = "doesnotexist"))
  # clean up
  invisible(tryCatch(docdb_delete(con, "doesnotexist"), error = function(e) e))
})

context("mongodb: query")
test_that("query in mongo works", {
  skip_on_cran()

  skip_if_no_mongo()
  con <- src_mongo(collection = "mtcars")

  invisible(docdb_create(con, "mtcars", mtcars))

  expect_message(
    docdb_query(con, "notmtcars", query = '{"mpg":21}'),
    "Parameter 'key' is different from parameter 'collection'")

  expect_is(suppressWarnings(
    docdb_query(con, "mtcars", query = '{"mpg":21}')),
    "data.frame")

  expect_is(suppressWarnings(
    docdb_query(con, query = '{"mpg":21}', "mtcars", fields = '{"mpg":1, "cyl":1}')),
    "data.frame"
  )
  # clean up
  invisible(tryCatch(docdb_delete(con, "mtcars"), error = function(e) e))
})

context("mongodb: update")
test_that("update in mongo works", {
  skip_on_cran()

  skip_if_no_mongo()
  con <- src_mongo(collection = "mtcars")

  # clean up
  invisible(tryCatch(docdb_delete(con, "mtcars"), error = function(e) e))

  con <- src_mongo(collection = "mtcars")

  # add data frame into table,
  # with user-provided _id
  # for subsequent reference
  # in update command
  invisible(
    docdb_create(con, "mtcars",
                 data.frame("_id" = seq_len(nrow(mtcars)),
                            mtcars,
                            stringsAsFactors = FALSE,
                            # to have _id as column name
                            check.names = FALSE)
    ))

  ## test with _id in replacement path

  # update - change value of existing key
  value <- data.frame("_id" = "2",
                      "gear" = 9,
                      stringsAsFactors = FALSE,
                      # to have _id as column name
                      check.names = FALSE)

  expect_equal(docdb_update(con, "mtcars", value), 1L)
  tmp <- docdb_query(con, "mtcars", query = "{}", fields = '{"gear": 1}')
  expect_equal(tmp[["gear"]][tmp[["_id"]] == "2"], 9)

  # upsert - add key and value not previously present
  value <- data.frame("_id" = "4",
                      "a" = 123,
                      stringsAsFactors = FALSE,
                      # to have _id as column name
                      check.names = FALSE)

  expect_equal(docdb_update(con, "mtcars", value), 1L)
  expect_equal(docdb_query(con, "mtcars", query = '{"_id": "4"}', fields = '{"a": 1}')[["a"]], 123)

  # multiple - upsert
  value <- data.frame("_id" = c("3", "5"),
                      "gear" = c(99, 66),
                      "cyl" = c(88, 77),
                      stringsAsFactors = FALSE,
                      # to have _id as column name
                      check.names = FALSE)

  expect_equal(docdb_update(con, "mtcars", value), 2L) # sqlite: 4L (counts operations, not rows)
  tmp <- docdb_query(con, "mtcars", query = "{}", fields = '{"gear": 1, "cyl": 1}')
  expect_equal(tmp[["cyl"]][tmp[["_id"]] == "5"], 77)

  ## test other paths than _id
  value <- data.frame("gear" = c(4, 5),
                      "carb" = c(8.1, 7.9),
                      stringsAsFactors = FALSE)

  expect_equal(docdb_update(con, "mtcars", value), 15L)
  tmp <- docdb_query(con, "mtcars", query = '{"gear": 4}', fields = '{"gear": 1, "carb": 1}')
  expect_true(all(tmp[["carb"]][tmp[["gear"]] == 4L] == 8.1))

  # clean up
  invisible(tryCatch(docdb_delete(con, "mtcars"), error = function(e) e))
})
