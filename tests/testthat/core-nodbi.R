# this file is called by each of the
# test_<database>.R files in order to
# canonically test nodbi core functions

#### create, get, exists, delete ####
test_that("docdb_create, docdb_exists, docdb_list, docdb_get, docdb_delete", {

  # get db connection
  tmp <- dbSrcKey()
  src <- tmp$testSrc
  key <- tmp$testKey
  on.exit(try({
    docdb_delete(src = src, key = key)
    if (any(c(inherits(src, "src_sqlite"), inherits(src, "src_postgres")))) DBI::dbDisconnect(src$con, shutdown = TRUE)
    if (inherits(src, "src_duckdb")) duckdb::dbDisconnect(src$con, shutdown = TRUE)
    rm(src, key, tmp)
  }, silent = TRUE), add = TRUE)

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
  expect_warning(suppressMessages(docdb_create(src = src, key = key, value = testJson)), "index|conflict|constraint|updated|duplicate|error|rapi") # _id violation
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
  expect_true(any( # uuid may be random, cannot expect rows and rownames to correspond
    setequal(unlist(docdb_get(src = src, key = key)[["rows"]][[1]]),
             unlist(jsonlite::fromJSON(mapdata, simplifyVector = TRUE)[["rows"]][[2]])),
    setequal(unlist(docdb_get(src = src, key = key)[["rows"]][[2]]),
             unlist(jsonlite::fromJSON(mapdata, simplifyVector = TRUE)[["rows"]][[2]]))
  ))
  expect_equal(suppressMessages(docdb_create(src = src, key = key, value = testJson2)), 2L)
  if (inherits(src, "src_elastic")) Sys.sleep(elasticSleep)
  expect_identical(nrow(docdb_get(src = src, key = key)), 4L)
  expect_true(docdb_delete(src = src, key = key))

  # testList
  expect_equal(docdb_create(src = src, key = key, value = testList), 2L)
  if (inherits(src, "src_elastic")) Sys.sleep(elasticSleep)
  expect_identical(sort(unlist(docdb_get(src = src, key = key)[, -1], use.names = FALSE)), sort(unlist(testList, use.names = FALSE)))

  # clean up
  expect_true(docdb_delete(src = src, key = key))
  expect_false(docdb_delete(src = src, key = key))

})


#### create (ndjson) ####
test_that("docdb_create (ndjson)", {

  # get db connection
  tmp <- dbSrcKey()
  src <- tmp$testSrc
  key <- tmp$testKey
  on.exit(try({
    docdb_delete(src = src, key = key)
    if (any(c(inherits(src, "src_sqlite"), inherits(src, "src_postgres")))) DBI::dbDisconnect(src$con, shutdown = TRUE)
    if (inherits(src, "src_duckdb")) duckdb::dbDisconnect(src$con, shutdown = TRUE)
    rm(src, key, tmp)
  }, silent = TRUE), add = TRUE)

  # get temporary local files with ndjson
  tF <- testFile()
  tF2 <- testFile2()

  # tests
  expect_equal(docdb_create(src = src, key = key, value = tF), 5L)
  expect_equal(suppressWarnings(docdb_create(src = src, key = key, value = tF)), 0L)
  expect_equal(docdb_create(src = src, key = key, value = tF2), nrow(diamonds))
  expect_equal(docdb_create(src = src, key = key, value = testUrl), 98L)

  # clean up
  expect_true(docdb_delete(src = src, key = key))
  expect_false(docdb_delete(src = src, key = key))

})


#### query ####
test_that("docdb_query", {

  tmp <- dbSrcKey()
  src <- tmp$testSrc
  key <- tmp$testKey
  on.exit(try({
    docdb_delete(src = src, key = key)
    if (any(c(inherits(src, "src_sqlite"), inherits(src, "src_postgres")))) DBI::dbDisconnect(src$con, shutdown = TRUE)
    if (inherits(src, "src_duckdb")) duckdb::dbDisconnect(src$con, shutdown = TRUE)
    rm(src, key, tmp)
  }, silent = TRUE), add = TRUE)

  # testJson
  expect_equal(docdb_create(src = src, key = key, value = testJson), 5L)
  if (inherits(src, "src_elastic")) Sys.sleep(elasticSleep)

  if (!inherits(src, "src_elastic")) expect_error(docdb_query(src = src, key = key, query = '{"$or": [{"$or": {"gear": 4}}, {"cyl": 6}]}'))

  if (!inherits(src, "src_elastic")) expect_equal(dim(docdb_query(src = src, key = key, query = '{}')), c(5L, 11L))
  if (!inherits(src, "src_elastic")) expect_equal(dim(docdb_query(src = src, key = key, query = '{}', fields = '{}')), c(5L, 11L))
  if (!inherits(src, "src_elastic")) expect_equal(dim(docdb_query(src = src, key = key, query = '{}', fields = '{"email": 1, "_id": 1}')), c(5L, 2L))
  if (!inherits(src, "src_elastic")) expect_equal(dim(docdb_query(src = src, key = key, query = '{"_id":"5cd67853f841025e65ce0ce2"}', fields = '{"email": 1}')), c(1L, 1L))
  if (!inherits(src, "src_elastic")) expect_equal(dim(docdb_query(src = src, key = key, query = '{"email": {"$regex": "lacychen@conjurica.com"}}')), c(1L, 11L))
  if (!inherits(src, "src_elastic")) expect_equal(dim(docdb_query(src = src, key = key, query = '{"email": {"$regex": "^lacychen"}}')), c(1L, 11L))
  if (!inherits(src, "src_elastic")) expect_equal(dim(docdb_query(src = src, key = key, query = '{"name": {"$ne": "Lacy Chen"}}')), c(4L, 11L))
  if (!inherits(src, "src_elastic")) expect_equal(dim(docdb_query(src = src, key = key, query = '{"name": {"$regex": "^[a-zA-Z]{3,4} "}}', fields = '{"name": 1, "age": 1}')), c(3L, 2L))

  if (!inherits(src, "src_couchdb") & !inherits(src, "src_elastic")) expect_equal(dim(docdb_query(src = src, key = key, query = '{"$and": [{"email": {"$regex": "ac"}}, {"friends.name": "Dona Bartlett"}]}')), c(1L, 11L))
  if (!inherits(src, "src_couchdb") & !inherits(src, "src_elastic")) expect_equal(dim(docdb_query(src = src, key = key, query = '{"$and": [{"_id": {"$regex": "53"}}, {"friends.name": "Dona Bartlett"}]}')), c(1L, 11L))
  if (!inherits(src, "src_couchdb") & !inherits(src, "src_elastic")) expect_equal(dim(docdb_query(src = src, key = key, query = '{"$and": [{"_id": {"$regex": "53"}}, {"friends.name": {"$regex": "Ba|La"}}]}')), c(2L, 11L))
  if (!inherits(src, "src_couchdb") & !inherits(src, "src_elastic")) expect_equal(dim(docdb_query(src = src, key = key, query = '{"$and": [{"_id": {"$regex": "53"}}, {"friends.name": "Dona Bartlett"}]}')), c(1L, 11L))
  if (!inherits(src, "src_couchdb") & !inherits(src, "src_elastic")) expect_equal(dim(docdb_query(src = src, key = key, query = '{"$or": [{"email": {"$regex": "ac"}}, {"friends.name": "Pace Bell"}]}')), c(3L, 11L))
  if (!inherits(src, "src_couchdb") & !inherits(src, "src_elastic")) expect_true(docdb_query(src = src, key = key, query = '{"friends.name": "Dona Bartlett"}', fields = '{"name": 1}')[["name"]] == "Pace Bell")
  if (!inherits(src, "src_couchdb") & !inherits(src, "src_elastic")) expect_equal(dim(docdb_query(src, key, query = '{"$or": [{"age": {"$gt": 21}}, {"friends.name": {"$regex": "^B[a-z]{3,9}.*"}}]}', fields = '{"age": 1, "friends.name": 1}')), c(3L, 2L))
  if (!inherits(src, "src_couchdb") & !inherits(src, "src_elastic")) expect_equal(dim(docdb_query(src = src, key = key, query = '{"$or": [{"email": {"$regex": "^lacychen"}}, {"friends.name": "Dona Bartlett"}]}')), c(2L, 11L))

  expect_equal(dim(docdb_query(src = src, key = key, query = '{"name": "Lacy Chen"}')), c(1L, 11L))
  expect_equal(dim(docdb_query(src = src, key = key, query = '{"age": 20}')), c(2L, 11L))
  expect_equal(dim(docdb_query(src = src, key = key, query = '{"age": 20}', fields = '{"name": 1, "age": 0, "_id": 1}')), c(2L, 2L))
  expect_equal(dim(docdb_query(src = src, key = key, query = '{"age": 20}', fields = '{"name": 0}')), c(2L, 10L))
  expect_equal(dim(docdb_query(src = src, key = key, query = '{"age": 20}', fields = '{"_id": 1, "friends": 1}')), c(2L, 2L))
  expect_equal(dim(docdb_query(src = src, key = key, query = '{"age": 20}', fields = '{"_id": 1, "friends.id": 1}')), c(2L, 2L)) # full friends field for couchdb, elasticsearch
  expect_true(nrow(docdb_query(src = src, key = key, query = '{"age": 20}', fields = '{"_id": 1, "age": 1, "doesnotexist": 1}')) == 2L)
  expect_true(ncol(docdb_query(src = src, key = key, query = '{"age": 20}', fields = '{"_id": 1, "age": 1, "doesnotexist": 1}')) <= 3L)
  expect_equal(dim(docdb_query(src = src, key = key, query = '{"email": "lacychen@conjurica.com"}')), c(1L, 11L))

  # anomaly that is very difficult to correct, nothing returned for non-existing field by RSQLite
  if (!inherits(src, "src_sqlite")) expect_equal(dim(docdb_query(src = src, key = key, query = '{"age": 20}', fields = '{"_id": 1, "doesnotexist": 1}')), c(2L, 1L))

    # couchdb cannot search in array
  if (!inherits(src, "src_elastic") & !inherits(src, "src_couchdb")) expect_equal(dim(
    docdb_query(src = src, key = key, query = '{"tags": {"$regex": "^[a-z]{3,4}$"}}', fields = '{"name": 1, "age": 1}')), c(3L, 2L))
  expect_true(docdb_delete(src = src, key = key))

  # remainder skipped for Elasticsearch until queries implemented in nodbi::docdb_query.src_elastic()
  if (inherits(src, "src_elastic")) skip("queries need to be translated into elastic syntax")

  # testJson2
  expect_equal(docdb_create(src = src, key = key, value = testJson2), 2L)
  expect_equal(dim(docdb_query(src = src, key = key, query = '{}', fields = '{"rows.elements.distance.somevalue": 1}')), c(2L, 1L))
  expect_equal(nrow(docdb_query(src = src, key = key, query = '{}', fields = '{"destination_addresses": 1}')), 2L)
  expect_equal(length(unlist(docdb_query(
    src = src, key = key, query = '{"origin_addresses": {"$in": ["Santa Barbara, CA, USA"]}}', fields = '{"destination_addresses": 1}'))), 3L)
  # note: str, typeof differ by database backend
  expect_true(docdb_delete(src = src, key = key))

  # testDf
  expect_equal(docdb_create(src = src, key = key, value = testDf), 32L)
  expect_equal(dim(docdb_query(src = src, key = key, query = '{}')), c(32L, 12L))
  expect_equal(dim(docdb_query(src = src, key = key, query = '{}', fields = '{"_id": 1}')), c(32L, 1L))
  expect_equal(dim(docdb_query(src, key, query = '{"gear":4, "mpg": {"$lte": 21.9}}')), c(5L, 12L))
  expect_equal(dim(docdb_query(src, key, query = '{"gear": {"$in": [5,4]}}')), c(17L, 12L))
  expect_equal(dim(docdb_query(src, key, query = '{"_id": {"$in": ["Datsun 710", "Merc 280C"]}}')), c(2L, 12L))
  expect_equal(dim(docdb_query(src = src, key = key, query = '{"mpg": {"$lte": 18.9}}', fields = '{"mpg": 1, "carb": 1}')), c(15L, 2L)) # mpg is not integer
  expect_equal(sum(docdb_query(src = src, key = key, query = '{"disp": {"$gt": 350}}', fields = '{"carb": 1, "_id": 0}')[[1]]), 24L)
  expect_identical(docdb_query(src = src, key = key, query = '{"$and": [{"mpg": {"$lte": 18}}, {"gear": {"$gt": 3}}]}'),
                   docdb_query(src = src, key = key, query = '          {"mpg": {"$lte": 18},   "gear": {"$gt": 3}}'))
  expect_equal(nrow(docdb_query(src = src, key = key, query = '{"$or": [{"mpg": {"$lte": 18.0}}, {"_id": {"$regex": "^F[a-z].*", "$options": ""}}]}')), 16L)

  # clean up
  expect_true(docdb_delete(src = src, key = key))

})

#### update ####
test_that("docdb_update, docdb_query", {

  tmp <- dbSrcKey()
  src <- tmp$testSrc
  key <- tmp$testKey
  on.exit(try({
    docdb_delete(src = src, key = key)
    if (any(c(inherits(src, "src_sqlite"), inherits(src, "src_postgres")))) DBI::dbDisconnect(src$con, shutdown = TRUE)
    if (inherits(src, "src_duckdb")) duckdb::dbDisconnect(src$con, shutdown = TRUE)
    rm(src, key, tmp)
  }, silent = TRUE), add = TRUE)

  expect_equal(docdb_create(src = src, key = key, value = testDf), nrow(testDf))
  if (inherits(src, "src_elastic")) Sys.sleep(elasticSleep)

  # tests0
  if (!inherits(src, "src_elastic")) expect_equal(docdb_update(src = src, key = key, value = '{"vs": 77}', query = '{"gear": {"$in": [5,4]}}'), 17L)
  if (!inherits(src, "src_elastic")) expect_true(all(docdb_query(src, key, query = '{"gear": {"$in": [5,4]}}', fields = '{"vs": 1}')[["vs"]] == 77L))

  # tests1
  expect_equal(docdb_update(src = src, key = key, value = mtcars[3, 4:5], query = '{"gear": 3}'), 15L) # hp = 93, drat = 3.9
  expect_true(all(docdb_query(src, key, query = '{"gear": 3}', fields = '{"hp": 1}')[["hp"]] == 93L))
  expect_true(mean(docdb_query(src, key, query = '{"gear": 4}', fields = '{"drat": 1}')[["drat"]]) > 3.9)

  # tests2
  expect_equal(docdb_update(src = src, key = key, value = mtcars[1:2, 4:5], query = '{"gear": 9999}'), 0L)
  expect_equal(docdb_update(src = src, key = key, value = mtcars[1:2, 4:5], query = '{"gear": 3}'), 15L)
  expect_equal(docdb_query(src, key, query = '{"gear": 3}', fields = '{"hp": 1}')[1, "hp", drop = TRUE], list(c(110, 110)))

  # tests3
  expect_equal(docdb_update(src = src, key = key, value = list(carb = 99L), query = '{"gear": 4}'), 12L)
  expect_true(all(docdb_query(src, key, query = '{"gear": 4}', fields = '{"carb": 1}')[["carb"]] == 99L))

  # tests4
  expect_equal(docdb_update(src = src, key = key, value = '{"vs": 9}', query = '{"gear": 5}'), 5L)
  expect_true(all(docdb_query(src, key, query = '{"gear": 5}', fields = '{"vs": 1}')[["vs"]] == 9L))

  # clean up
  expect_true(docdb_delete(src = src, key = key))

})
