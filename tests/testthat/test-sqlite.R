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
  # remove _id column
  d2 <- d2[, -1]
  
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
      docdb_query(con, "mtcars", query = "{}", fields = '{"mpg":1, "cyl": 1}'),
    "data.frame")

  expect_length(
      docdb_query(con, "mtcars", 
                  query = '{"gear" : 4, "cyl": {"$lte": 8}, "_row": {"$regex": "M%"} }', 
                  fields = '{"mpg":1, "cyl": 1, "_row": 1}')[["_id"]],
    6L)

  invisible(docdb_create(con, "mtcars", contacts))
  
  expect_equal(
    docdb_query(con, "mtcars", 
                fields = '{"age": 1, "name": 1}', 
                query = '{"name": "Lacy Chen", "age": {"$lt": 22}}')[["_id"]],
    "5cd678531b423d5f04cfb0a1")
  
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
  expect_equal(docdb_query(con, "mtcars", query = "{}", fields = '{"a": 1}')[["a"]], 123)

  # multiple - upsert
  value <- data.frame("_id" = c("3", "5"), 
                      "gear" = c(8, 7), 
                      stringsAsFactors = FALSE,
                      # to have _id as column name
                      check.names = FALSE) 
  
  expect_equal(docdb_update(con, "mtcars", value), 2L)
  tmp <- docdb_query(con, "mtcars", query = "{}", fields = '{"gear": 1}')
  expect_equal(tmp[["gear"]][tmp[["_id"]] == "3"], 8)
  
  ## test other paths than _id
  value <- data.frame("gear" = c("4", "5"),
                      "a" = c(8, 7),
                      stringsAsFactors = FALSE)
  
  expect_equal(docdb_update(con, "mtcars", value), 15L)
  tmp <- docdb_query(con, "mtcars", query = "{}", fields = '{"gear": 1, "a": 1}')
  expect_true(all(tmp[["a"]][tmp[["gear"]] == 4] == 8))
  
})
