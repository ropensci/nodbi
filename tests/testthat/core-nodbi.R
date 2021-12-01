# this file is called by each of the
# test_<database>.R files in order to
# canonically test nodbi core functions

#### test data ####
testDf <- mtcars # has rownames
testDf2 <- iris # no rownames
testJson <- contacts # has _id's
testJson2 <- mapdata # no _id's
testList <- jsonlite::fromJSON(mapdata, simplifyVector = FALSE)
# factors cannot be expected to be maintained
testDf2[["Species"]] <- as.character(testDf2[["Species"]])

#### set up ####
elasticSleep <- 1L # seconds


#### create get delete ####
context("- create, exists, list, get, delete")
test_that("docdb_create, docdb_exists, docdb_list, docdb_get, docdb_delete", {

  # get db connection
  tmp <- dbSrcKey()
  src <- tmp$testSrc
  key <- tmp$testKey
  on.exit(rm(src), add = TRUE)

  # clean up databases
  if (FALSE) {
    for (db in docdb_list(src)) {src$con$run(paste0('{"drop":"', db,'"}'))} # mongo
    for (db in docdb_list(src)) {docdb_delete(src, db)} # all others
  }

  # testDf
  expect_equal(docdb_create(src = src, key = key, value = testDf), nrow(testDf))
  expect_message(docdb_create(src = src, key = key, value = testDf[0, ]), "already exists")
  if (inherits(src, "src_elastic")) Sys.sleep(elasticSleep)
  if (inherits(src, "src_postgres")) expect_equal(dim(docdb_get(src = src, key = key)), c(32L, 12L))
  if (!inherits(src, "src_postgres")) expect_equal(docdb_get(src = src, key = key)[, -1], `rownames<-`(testDf[order(row.names(testDf)), ], NULL))
  expect_true(docdb_delete(src = src, key = key))
  expect_false(docdb_exists(src = src, key = key))
  expect_false(any(docdb_list(src = src) == key))

  # testDf2
  if (inherits(src, "src_mongo")) names(testDf2) <- gsub("[.]", "_", names(testDf2))
  expect_equal(docdb_create(src = src, key = key, value = testDf2), nrow(testDf2))
  expect_true(docdb_exists(src = src, key = key))
  expect_true(any(docdb_list(src = src) == key))
  if (inherits(src, "src_elastic")) Sys.sleep(elasticSleep)
  expect_identical(sort(names(docdb_get(src = src, key = key)[, -1])), sort(names(testDf2)))
  expect_identical(dim(docdb_get(src = src, key = key)[, -1]), dim(testDf2))
  expect_true(docdb_delete(src = src, key = key))

  # testJson
  expect_equal(docdb_create(src = src, key = key, value = testJson), 5L)
  expect_warning(suppressMessages(docdb_create(src = src, key = key, value = testJson)), "index|conflict|constraint|updated|duplicate|error") # _id violation
  if (inherits(src, "src_elastic")) Sys.sleep(elasticSleep)
  if (inherits(src, "src_postgres")) expect_equal(dim(docdb_get(src = src, key = key)), c(5L, 11L))
  if (!inherits(src, "src_postgres")) expect_identical(docdb_get(src = src, key = key),
                                                       `rownames<-`(jsonlite::fromJSON(testJson)[order(jsonlite::fromJSON(testJson)[["_id"]]), ], NULL))
  expect_true(all(vapply(docdb_get(src = src, key = key)[["friends"]], is.data.frame, is.logical(1L)))) # same nesting
  expect_true(docdb_delete(src = src, key = key, query = '{"email": "lacychen@conjurica.com"}'))
  expect_false(docdb_delete(src = src, key = key, query = '{"email": "lacychen@conjurica.com"}')) # second delete
  expect_true(docdb_delete(src = src, key = key))

  # testJson2
  expect_equal(docdb_create(src = src, key = key, value = testJson2), 2L)
  if (inherits(src, "src_elastic")) Sys.sleep(elasticSleep)
  if (!inherits(src, "src_postgres")) expect_identical(docdb_get(src = src, key = key)[["rows"]][[2]], jsonlite::fromJSON(mapdata, simplifyVector = TRUE)[["rows"]][[2]])
  expect_identical(sort(unlist(docdb_get(src = src, key = key)[, -1])), sort(unlist(jsonlite::fromJSON(mapdata, simplifyVector = TRUE))))
  expect_equal(suppressMessages(docdb_create(src = src, key = key, value = testJson2)), 2L)
  if (inherits(src, "src_elastic")) Sys.sleep(elasticSleep)
  expect_identical(nrow(docdb_get(src = src, key = key)), 4L)
  expect_true(docdb_delete(src = src, key = key))

  # testList
  expect_equal(docdb_create(src = src, key = key, value = testList), 2L)
  if (inherits(src, "src_elastic")) Sys.sleep(elasticSleep)
  expect_identical(sort(unlist(docdb_get(src = src, key = key)[, -1], use.names = FALSE)), sort(unlist(testList, use.names = FALSE)))
  expect_true(docdb_delete(src = src, key = key))

})

#### query ####
context("- query")
test_that("docdb_query", {

  tmp <- dbSrcKey()
  src <- tmp$testSrc
  key <- tmp$testKey
  on.exit(rm(src), add = TRUE)

  # testJson
  expect_equal(docdb_create(src = src, key = key, value = testJson), 5L)
  if (inherits(src, "src_elastic")) Sys.sleep(elasticSleep)
  expect_equal(dim(docdb_query(src = src, key = key, query = '{"name": "Lacy Chen"}')), c(1L, 11L))
  expect_equal(dim(docdb_query(src = src, key = key, query = '{"age": 20}')), c(2L, 11L))
  if (!inherits(src, "src_couchdb")) expect_true(docdb_query(src = src, key = key, query = '{"friends.name": "Dona Bartlett"}', fields = '{"name": 1}')[["name"]] == "Pace Bell")
  expect_equal(dim(docdb_query(src = src, key = key, query = '{"age": 20}', fields = '{"name": 1, "age": 0, "_id": 1}')), c(2L, 2L))
  expect_equal(dim(docdb_query(src = src, key = key, query = '{"age": 20}', fields = '{"name": 0}')), c(2L, 10L))
  expect_equal(dim(docdb_query(src = src, key = key, query = '{"age": 20}', fields = '{"_id": 1, "friends": 1}')), c(2L, 2L))
  expect_equal(dim(docdb_query(src = src, key = key, query = '{"age": 20}', fields = '{"_id": 1, "friends.id": 1}')), c(2L, 2L)) # full friends field for couchdb, elasticsearch
  expect_true(nrow(docdb_query(src = src, key = key, query = '{"age": 20}', fields = '{"_id": 1, "age": 1, "doesnotexist": 1}')) == 2L)
  expect_true(ncol(docdb_query(src = src, key = key, query = '{"age": 20}', fields = '{"_id": 1, "age": 1, "doesnotexist": 1}')) <= 3L)
  # abnomaly that is very difficult to correct, nothing returned for non-existing field by RSQLite
  if (inherits(src, "src_sqlite")) expect_equal(dim(docdb_query(src = src, key = key, query = '{"age": 20}', fields = '{"_id": 1, "doesnotexist": 1}')), c(0L, 0L))
  if (!inherits(src, "src_sqlite")) expect_equal(nrow(docdb_query(src = src, key = key, query = '{"age": 20}', fields = '{"_id": 1, "doesnotexist": 1}')), 2L)
  # skip remainder for Elasticsearch
  if (!inherits(src, "src_elastic")) expect_equal(dim(docdb_query(src = src, key = key, query = '{"name": {"$ne": "Lacy Chen"}}')), c(4L, 11L))
  expect_true(docdb_delete(src = src, key = key))
  if (inherits(src, "src_elastic")) skip("queries need to be translated into elastic syntax")

  # testJson2
  expect_equal(docdb_create(src = src, key = key, value = testJson2), 2L)
  if (inherits(src, "src_elastic")) Sys.sleep(elasticSleep)
  expect_equal(dim(docdb_query(src = src, key = key, fields = '{"rows.elements.distance.somevalue": 1}', query = '{}')), c(2L, 1L))
  # note: str, typeof differ by database backend
  expect_true(docdb_delete(src = src, key = key))

  # testDf
  expect_equal(docdb_create(src = src, key = key, value = testDf), 32L)
  if (inherits(src, "src_elastic")) Sys.sleep(elasticSleep)
  expect_equal(dim(docdb_query(src, key, query = '{"gear": {"$in": [5,4]}}')), c(17L, 12L))
  expect_equal(dim(docdb_query(src, key, query = '{"_id": {"$in": ["Datsun 710", "Merc 280C"]}}')), c(2L, 12L))
  expect_equal(dim(docdb_query(src = src, key = key, query = '{"mpg": {"$lte": 18}}', fields = '{"disp": 1, "carb": 1}')), c(13L, 2L))
  expect_equal(sum(docdb_query(src = src, key = key, query = '{"disp": {"$gt": 350}}', fields = '{"carb": 1, "_id":0}')[[1]]), 24L)
  expect_identical(docdb_query(src = src, key = key, query = '{"$and": [{"mpg": {"$lte": 18}}, {"gear": {"$gt": 3}}]}'),
                   docdb_query(src = src, key = key, query = '          {"mpg": {"$lte": 18},   "gear": {"$gt": 3}}'))
  expect_equal(dim(docdb_query(src = src, key = key, query = '{}', fields = '{"_id": 1}')), c(32L, 1L))
  expect_equal(dim(docdb_query(src = src, key = key, query = '{}')), c(32L, 12L))
  expect_equal(nrow(docdb_query(src = src, key = key, query = '{"$or": [{"mpg": {"$lte": 18}}, {"_id": {"$regex": "^F[a-z].*", "$options": ""}}]}')), 16L)
  expect_equal(nrow(docdb_query(src = src, key = key, query = '{"$or": [{"mpg": {"$lte": 18}}, {"_id": {"$regex": "^F[a-z].*"}}]}')), 16L)
  expect_error(docdb_get(src = src, key = key, query = '{"mpg": {"$lte": 18}}', fields = '{"mpg":1, "cyl":1, "_id": 0}'), "docdb_query")
  expect_true(docdb_delete(src = src, key = key))

})

#### update ####
context("- update, query")
test_that("docdb_update", {

  tmp <- dbSrcKey()
  src <- tmp$testSrc
  key <- tmp$testKey
  on.exit(rm(src), add = TRUE)

  expect_equal(docdb_create(src = src, key = key, value = testDf), nrow(testDf))
  if (inherits(src, "src_elastic")) Sys.sleep(elasticSleep)
  #
  expect_equal(docdb_update(src = src, key = key, value = mtcars[3, 4:5], query = '{"gear": 3}'), 15L) # hp = 93, drat = 3.9
  expect_true(all(docdb_query(src, key, query = '{"gear": 3}', fields = '{"hp": 1}')[["hp"]] == 93L))
  expect_true(mean(docdb_query(src, key, query = '{"gear": 4}', fields = '{"drat": 1}')[["drat"]]) > 3.9)
  #
  expect_equal(docdb_update(src = src, key = key, value = mtcars[1:2, 4:5], query = '{"gear": 9999}'), 0L)
  expect_equal(docdb_update(src = src, key = key, value = mtcars[1:2, 4:5], query = '{"gear": 3}'), 15L)
  expect_equal(docdb_query(src, key, query = '{"gear": 3}', fields = '{"hp": 1}')[1, "hp", drop = TRUE], list(c(110, 110)))
  #
  expect_equal(docdb_update(src = src, key = key, value = list(carb = 99L), query = '{"gear": 4}'), 12L)
  expect_true(all(docdb_query(src, key, query = '{"gear": 4}', fields = '{"carb": 1}')[["carb"]] == 99L))
  #
  expect_equal(docdb_update(src = src, key = key, value = '{"vs": 9}', query = '{"gear": 5}'), 5L)
  expect_true(all(docdb_query(src, key, query = '{"gear": 5}', fields = '{"vs": 1}')[["vs"]] == 9L))
  #
  if (!inherits(src, "src_elastic")) expect_equal(docdb_update(src = src, key = key, value = '{"vs": 77}', query = '{"gear": {"$in": [5,4]}}'), 17L)
  if (!inherits(src, "src_elastic")) expect_true(all(docdb_query(src, key, query = '{"gear": {"$in": [5,4]}}', fields = '{"vs": 1}')[["vs"]] == 77L))
  #
  expect_true(docdb_delete(src = src, key = key))

})
