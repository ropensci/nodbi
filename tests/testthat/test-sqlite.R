context("sqlite: src")
test_that("Source", {
  skip_on_cran()
  
  skip_if_no_sqlite()
  src <- src_sqlite()
  expect_is(src, "docdb_src")
  expect_is(src, "src_sqlite")
  expect_is(src$con, "SQLiteConnection")
})

context("sqlitedb: create")
test_that("db into sqlite", {
  skip_on_cran()
  
  skip_if_no_sqlite()
  con <- src_sqlite()
  # delete if exists
  invisible(tryCatch(docdb_delete(con, "iris"), error = function(e) e))
  
  iris$Species <- as.character(iris$Species)
  invisible(docdb_create(con, "iris", iris))
  d2 <- docdb_get(con, "iris")
  
  expect_equal(d2, iris)
})

context("sqlitedb: delete")
test_that("delete in sqlite works", {
  skip_on_cran()
  
  skip_if_no_sqlite()
  con <- src_sqlite()
  # delete if exists
  invisible(tryCatch(docdb_delete(con, "iris"), error = function(e) e))
  
  invisible(docdb_create(con, "iris", iris))
  del <- docdb_delete(con, "iris")
  expect_true(del)
})

context("sqlitedb: exists")
test_that("exists in sqlite works", {
  skip_on_cran()
  
  skip_if_no_sqlite()
  con <- src_sqlite(collection = "iris")
  # delete if exists
  invisible(tryCatch(docdb_delete(con, "iris"), error = function(e) e))
  
  invisible(docdb_create(con, "iris", iris))
  
  expect_true(docdb_exists(con, "iris"))
  expect_false(docdb_exists(con, "doesnotexist"))
})

context("sqlitedb: query")
test_that("query in sqlite works", {
  skip_on_cran()
  
  skip_if_no_sqlite()
  con <- src_sqlite()
  # delete if exists
  invisible(tryCatch(docdb_delete(con, "mtcars"), error = function(e) e))
  
  invisible(docdb_create(con, "mtcars", mtcars))
  
  expect_is(
    suppressWarnings(
      docdb_query(con, "mtcars", fields = '{"mpg":1}')),
    "data.frame")
  
  expect_is(
    suppressWarnings(
      docdb_query(con, "mtcars", fields = c("mpg", "cyl"))),
    "data.frame"
  )
  
})

context("sqlitedb: update")
test_that("update in sqlite works", {
  skip_on_cran()
  
  skip_if_no_sqlite()
  con <- src_sqlite()
  # delete if exists
  invisible(tryCatch(docdb_delete(con, "mtcars"), error = function(e) e))
  
  # add data frame into table, 
  # with user-provided _id
  # for subsequent reference
  # in update command
  invisible(docdb_create(con, "mtcars", 
                         data.frame("_id" = seq_len(nrow(mtcars)), 
                                    mtcars, 
                                    stringsAsFactors = FALSE,
                                    check.names = FALSE)   # to have _id as column name
                         ))
  
  ## test with _id in replacement path
  
  # update - change value of existing key
  value <- data.frame("_id" = "2", 
                      "gear" = 9,
                      stringsAsFactors = FALSE, 
                      check.names = FALSE) # to have _id as column name

  expect_equal(docdb_update(con, "mtcars", value), 1L)
  tmp <- docdb_query(con, "mtcars", fields = "gear")
  expect_equal(tmp[["gear"]][tmp[["_id"]] == "2"], "9")
  
  # upsert - add key and value not previously present
  value <- data.frame("_id" = "4", 
                      "a" = 123,
                      stringsAsFactors = FALSE, 
                      check.names = FALSE) # to have _id as column name
  
  expect_equal(docdb_update(con, "mtcars", value), 1L)
  expect_equal(docdb_query(con, "mtcars", fields = "a")[["a"]], "123")
  
  # multiple - upsert
  value <- data.frame("_id" = c("3", "5"), 
                      "gear" = c(8, 7), 
                      stringsAsFactors = FALSE, 
                      check.names = FALSE) # to have _id as column name
  
  expect_equal(docdb_update(con, "mtcars", value), 2L)
  tmp <- docdb_query(con, "mtcars", fields = "gear")
  expect_equal(tmp[["gear"]][tmp[["_id"]] == c("3", "5")], c("8", "7"))
  
  ## test other paths than _id
  value <- data.frame("gear" = c("4", "5"),
                      "a" = c(8, 7),
                      stringsAsFactors = FALSE)
  
  expect_equal(docdb_update(con, "mtcars", value), 15L)
  tmp <- docdb_query(con, "mtcars", fields = c("gear", "a"))
  expect_true(all(tmp[["a"]][tmp[["gear"]] == "4"] == "8"))
  
})
