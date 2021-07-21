context("sqlitedb: src")
test_that("Source", {
  skip_on_cran()

  skip_if_no_sqlite()
  con <- src_sqlite()
  on.exit(RSQLite::dbDisconnect(con$con), add = TRUE)

  expect_is(con, "docdb_src")
  expect_is(con, "src_sqlite")
  expect_is(con$con, "SQLiteConnection")

  capture.output(suppressWarnings(print(con)))
})

context("sqlitedb: create")
test_that("db into sqlite", {
  skip_on_cran()

  skip_if_no_sqlite()
  con <- src_sqlite()
  on.exit(RSQLite::dbDisconnect(con$con), add = TRUE)

  # delete if exists
  invisible(tryCatch(docdb_delete(con, "iris"), error = function(e) e))

  # create empty table
  expect_equal(docdb_create(con, "iris", NULL), 0L)

  iris$Species <- as.character(iris$Species)
  docdb_create(con, "iris", iris)

  d2 <- docdb_get(con, "iris", limit = 1L)
  d2 <- docdb_get(con, "iris")
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

  invisible(tryCatch(docdb_delete(con, "iris"), error = function(e) e))

  # check if timing acceptable
  docdb_create(con, "diamonds", diamonds)
  invisible(tryCatch(docdb_delete(con, "diamonds"), error = function(e) e))

  # create json
  docdb_create(
    con, "mapdata",
    data.frame(
      "_id" = "someid",
      "otherjson" = mapdata,
      check.names = FALSE,
      stringsAsFactors = FALSE
    ))
  d2 <- docdb_get(con, "mapdata")
  d2 <- d2$rows[[1]]$elements[[1]]$duration$somevalue
  expect_identical(d2, c(14064L, 151772L, 67405L, 152913L))
  invisible(tryCatch(docdb_delete(con, "mapdata"), error = function(e) e))

})

context("sqlitedb: delete")
test_that("delete in sqlite works", {
  skip_on_cran()

  skip_if_no_sqlite()
  con <- src_sqlite()
  on.exit(RSQLite::dbDisconnect(con$con), add = TRUE)

  # delete if exists
  invisible(tryCatch(docdb_delete(con, "iris"), error = function(e) e))

  invisible(docdb_create(con, "iris", iris))

  expect_equal(
    docdb_delete(
      con, "iris",
      query = '{"_id": "1"}'), 1L)

  expect_equal(
    docdb_delete(
      con, "iris",
      query = '{"Species": "setosa"}'), 49L)

  del <- docdb_delete(con, "iris")
  expect_true(del)

})

context("sqlitedb: exists")
test_that("exists in sqlite works", {
  skip_on_cran()

  skip_if_no_sqlite()
  con <- src_sqlite()
  on.exit(RSQLite::dbDisconnect(con$con), add = TRUE)

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
  on.exit(RSQLite::dbDisconnect(con$con), add = TRUE)

  # delete if exists
  invisible(tryCatch(docdb_delete(con, "mt-cars"), error = function(e) e))
  docdb_create(con, "mt-cars", mtcars)

  expect_is(
    docdb_query(con, "mt-cars", query = "{}", fields = '{"mpg":1, "cyl": 1}'),
    "data.frame")

  # test limit
  expect_equal(nrow(docdb_query(
    src = con, key = "mt-cars",
    query = "{}",
    fields = '{}',
    limit = 12L)), 12L)

  # test non-existing fields
  expect_equal(names(docdb_query(
    src = con, key = "mt-cars",
    query = "{}",
    fields = '{"doesnotexist": 1, "_id": 1}',
    limit = 12L)), c( "_id", "doesnotexist"))

  # test if _id only works
  expect_equal(nrow(docdb_query(
    src = con, key = "mt-cars",
    query = "{}",
    fields = '{"_id": 1}')), 32L)

  # test if _id only works
  expect_equal(nrow(docdb_query(
    src = con, key = "mt-cars",
    query = '{"gear": 4}',
    fields = '{"_id": 1}')), 12L)

  # test if _id only works
  expect_equal(dim(docdb_query(
    src = con, key = "mt-cars",
    query = '{"gear": 4, "_id": {"$regex": "^M[a-z].*"}}',
    fields = '{"_id": 1}')), c(6L, 1L))

  # test regular expression operator
  expect_length(
    docdb_query(
      con, "mt-cars",
      query = '{"gear" : 4, "cyl": {"$lte": 8}, "_row": {"$regex": "^M[a-z].*"} }',
      fields = '{"mpg":1, "cyl": 1, "_row": 1}')[["_id"]],
    6L)

  # add contacts
  invisible(docdb_create(
    con, "mt-cars",
    value = data.frame(contacts, stringsAsFactors = FALSE)))

  # tests with names of all fields,
  # including item.subitem.subsubitem
  expect_equal(
    length(allfields <-
             docdb_query(con, "mt-cars",
                         listfields = TRUE,
                         query = "{}")),
    24L)
  expect_true(
    any(c("friends.name") == allfields))

  # remove field names from mtcars data set
  # and also the _row field = [-1] which resulted
  # from importing mtcars
  tmpfs <- allfields[!allfields %in% names(mtcars)][-1]
  expect_equal(
    dim(
      docdb_query(
        con, "mt-cars",
        fields = paste0(
          "{",
          paste0(
            '"',
            tmpfs,
            collapse = '": 1, '),
          '": 1}',
          collapse = ""
        ),
        query = "{}")), c(5L, length(tmpfs) + 1L)) # + 1L for _id column

  # since mongodb returns an empty column,
  # this behaviour is emulated for sqlite
  expect_true(
    all(is.na(docdb_query(
      con, "mt-cars",
      fields = '{"non.existing.field": 1}',
      query = "{}")[["non.existing.field"]])))

  # test nested elements
  expect_equal(
    dimnames(
      docdb_query(
        con, "mt-cars",
        fields = '{"friends.name": 1}',
        query = "{}")), list(as.character(1:5), c("_id", "friends.name")))

  # empty result (all NA)
  expect_equal(nrow(
    docdb_query(
      con, "mt-cars",
      fields = '{"age": 1, "name": 1}',
      # age of Lacy Chen is 23 years
      query = '{"name": "Lacy Chen", "age": {"$lt": 22}}')),
    0L)

  # all 5 contacts found
  expect_equal(nrow(
    docdb_query(
      con, "mt-cars",
      fields = '{"age": 1, "name": 1}',
      query = '{"$or": {"_id": "5cd6785335b63cb19dfa8347", "age": {"$lt": 25}}}')),
    5L)

  # test content of a single cell
  expect_equal(
    docdb_query(
      con, "mt-cars",
      fields = '{"age": 1, "friends.name": 1}',
      query = '{"$and": {"eyeColor": "green", "age": {"$lt": 25}}}')[["friends.name"]],
    list(c("Wooten Goodwin", "Brandie Woodward", "Angelique Britt")))

  # test atomic content of cell
  expect_equal(
    docdb_query(
      con, "mt-cars",
      fields = '{"age": 1, "name": 1}',
      query = '{"eyeColor": "green", "age": {"$lt": 25}}')[["name"]],
    "Lacy Chen")

  # test query and nested elements
  expect_setequal(
    unname(docdb_query(
      src = con, key = "mt-cars",
      fields = '{"age": 1, "friends.id": 1}',
      query = '{"_id": "5cd6785335b63cb19dfa8347"}')[, , drop = TRUE]),
    unname(data.frame(
      "_id" = "5cd6785335b63cb19dfa8347",
      "age" = 30L,
      "friends.id" = I(list(c(0., 1., 2.))),
      stringsAsFactors = FALSE,
      check.names = FALSE)[, , drop = TRUE]))

  # test further
  expect_setequal(
    docdb_query(
      src = con, key = "mt-cars",
      fields = '{"age": 1, "friends.id": 1, "tags": 1}',
      query = '{"_id": "5cd6785335b63cb19dfa8347"}')[["tags"]],
    list(c("exercitation", "do", "magna", "ut", "consectetur", "ex", "incididunt")))

  expect_true(
    is.data.frame(
      docdb_query(
        con, "mt-cars",
        fields = '{"age": 1, "friends": 1}',
        query = '{"_id": "5cd6785335b63cb19dfa8347"}')
    ))

  expect_equal(
    ncol(
      docdb_query(
        con, "mt-cars",
        fields = '{"age": 1, "friends": 1, "tags": 1}',
        query = '{"_id": "5cd6785335b63cb19dfa8347"}')
    ), 4L)

  expect_true(
    is.list(
      docdb_query(
        con, "mt-cars",
        fields = '{"age": 1, "friends": 1}',
        query = '{"_id": "5cd6785335b63cb19dfa8347"}')[["friends"]]
    ))

  expect_true(
    is.data.frame(
      docdb_query(
        con, "mt-cars",
        fields = '{"age": 1, "friends": 1}',
        query = '{"_id": "5cd6785335b63cb19dfa8347"}')[["friends"]][[1]]
    ))

  expect_equal(
    length(
      docdb_query(
        con, "mt-cars",
        fields = '{"age": 1, "friends": 1}',
        query = '{"_id": "5cd6785335b63cb19dfa8347"}')[["friends"]][[1]][["name"]]),
    3L)

})

context("sqlitedb: update")
test_that("update in sqlite works", {
  skip_on_cran()

  skip_if_no_sqlite()
  con <- src_sqlite()
  on.exit(RSQLite::dbDisconnect(con$con), add = TRUE)

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
  expect_equal(tmp[["gear"]][tmp[["_id"]] == "2"], 9L)

  # upsert - add key and value not previously present
  value <- data.frame("_id" = "4",
                      "a" = 123L,
                      stringsAsFactors = FALSE,
                      # to have _id as column name
                      check.names = FALSE)

  expect_equal(docdb_update(con, "mtcars", value), 1L)
  expect_equal(docdb_query(con, "mtcars", query = "{}", fields = '{"a": 1}')[["a"]], 123)

  # multiple - upsert
  value <- data.frame("_id" = c("3", "5"),
                      "gear" = c(99L, 66L),
                      "cyl" = c(88L, 77L),
                      stringsAsFactors = FALSE,
                      # to have _id as column name
                      check.names = FALSE)

  expect_equal(docdb_update(con, "mtcars", value), 2L)
  tmp <- docdb_query(con, "mtcars", query = "{}", fields = '{"gear": 1, "cyl": 1}')
  expect_equal(tmp[["cyl"]][tmp[["_id"]] == "5"], 77L)

  ## test other paths than _id
  value <- data.frame("gear" = c(3L, 4L, 5L),
                      "a" =    c(8L, 7L, NA),
                      "b" =    c("b1", NA, "b2"),
                      stringsAsFactors = FALSE)

  expect_equal(docdb_update(con, "mtcars", value), 29L)
  tmp <- docdb_get(con, "mtcars")
  expect_true(all(tmp[["a"]][tmp[["gear"]] == 3L] == 8L))
  expect_true(all(is.na(tmp[["a"]][tmp[["gear"]] == 5L])))

  ## test updating with json string
  value <- data.frame("carb" = 8L,
                      "json1" = '{"mpg": 123, "gear": 3}',
                      "json2" = '{"gear": 456}',
                      stringsAsFactors = FALSE)
  expect_equal(docdb_update(src = con, key = "mtcars", value = value), 1L)
  tmp <- docdb_get(con, "mtcars")
  expect_true(tmp[["carb"]][tmp[["mpg"]] == 123L] == 8L)
  expect_true(tmp[["gear"]][tmp[["mpg"]] == 123L] == 456L)

})
