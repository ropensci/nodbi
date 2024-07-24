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

  # no data
  expect_equal(docdb_create(src = src, key = key, value = data.frame()), 0L)
  tdf <- docdb_get(src = src, key = key)
  expect_true(is.null(tdf) || !nrow(tdf))
  # - mongo needs a document to create a collection thus this cannot be true
  if (!inherits(src, "src_mongo")) expect_true(docdb_delete(src = src, key = key))
  expect_false(docdb_delete(src = src, key = key))

  # testDf
  expect_equal(docdb_create(src = src, key = key, value = testDf), nrow(testDf))
  expect_message(docdb_create(src = src, key = key, value = testDf[0, ]), "already exists")
  expect_equal(dim(docdb_get(src = src, key = key)), c(32L, 12L))
  expect_equal(dim(docdb_get(src = src, key = key, limit = 1L)), c(1L, 12L))
  # - postgres does not maintain field order
  if (!inherits(src, "src_postgres")) expect_equal(
    docdb_get(src = src, key = key)[, -1], `rownames<-`(testDf[order(row.names(testDf)), ], NULL))
  expect_error(docdb_delete(src = src, key = key, query = '{"invalidjson'))
  expect_true(docdb_delete(src = src, key = key))
  expect_false(docdb_exists(src = src, key = key))
  expect_false(any(docdb_list(src = src) == key))

  # testDf2
  # - mongo cannot / shall not have dots in the name of a single field
  if (inherits(src, "src_mongo")) names(testDf2) <- gsub("[.]", "_", names(testDf2))
  expect_equal(docdb_create(src = src, key = key, value = testDf2), nrow(testDf2))
  expect_true(docdb_exists(src = src, key = key))
  expect_true(any(docdb_list(src = src) == key))
  expect_identical(sort(names(docdb_get(src = src, key = key)[, -1])), sort(names(testDf2)))
  expect_identical(dim(docdb_get(src = src, key = key)[, -1]), dim(testDf2))
  expect_true(docdb_delete(src = src, key = key))

  # testJson
  expect_equal(docdb_create(src = src, key = key, value = testJson), 5L)
  expect_warning(suppressMessages(docdb_create(src = src, key = key, value = testJson)), "index|conflict|constraint|updated|duplicate|error|rapi") # _id violation
  expect_equal(dim(docdb_get(src = src, key = key)), c(5L, 11L))
  if (!inherits(src, "src_postgres")) expect_identical(
    docdb_get(src = src, key = key),
    `rownames<-`(jsonlite::fromJSON(testJson)[order(jsonlite::fromJSON(testJson)[["_id"]]), ], NULL))
  expect_true(all(vapply(docdb_get(src = src, key = key)[["friends"]], is.data.frame, is.logical(1L)))) # same nesting
  expect_true(docdb_delete(src = src, key = key, query = '{"email": "lacychen@conjurica.com"}'))
  expect_false(docdb_delete(src = src, key = key, query = '{"email": "lacychen@conjurica.com"}')) # second delete
  expect_true(docdb_delete(src = src, key = key))

  # testJson2
  expect_equal(docdb_create(src = src, key = key, value = testJson2), 2L)
  expect_true(any( # uuid may be random, cannot expect rows and rownames to correspond
    setequal(unlist(docdb_get(src = src, key = key)[["rows"]][[1]]),
             unlist(jsonlite::fromJSON(mapdata, simplifyVector = TRUE)[["rows"]][[2]])),
    setequal(unlist(docdb_get(src = src, key = key)[["rows"]][[2]]),
             unlist(jsonlite::fromJSON(mapdata, simplifyVector = TRUE)[["rows"]][[2]]))
  ))
  expect_equal(suppressMessages(docdb_create(src = src, key = key, value = testJson2)), 2L)
  expect_identical(nrow(docdb_get(src = src, key = key)), 4L)
  expect_identical(nrow(docdb_get(src = src, key = key, limit = 2L)), 2L)
  expect_error(docdb_get(src = src, key = key, query = '{}'))
  expect_true(docdb_delete(src = src, key = key))

  # testList
  expect_equal(docdb_create(src = src, key = key, value = testList), 2L)
  expect_identical(sort(unlist(docdb_get(src = src, key = key)[, -1], use.names = FALSE)), sort(unlist(testList, use.names = FALSE)))

  # testDf3
  expect_equal(docdb_create(src = src, key = key, value = testDf3), 5L)
  expect_equal(docdb_create(src = src, key = key, value = cbind(`_id` = as.character(seq_len(nrow(testDf3))), testDf3)), 5L)
  expect_identical(nrow(docdb_get(src = src, key = key)), 12L)

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

  # clean up
  expect_true(docdb_delete(src = src, key = key))
  expect_false(docdb_delete(src = src, key = key))

  # test
  skip_if(is.null(httpbin), "package webfakes missing")
  expect_equal(docdb_create(src = src, key = key, value = httpbin$url("/stream/98")), 98L)
  expect_true(docdb_delete(src = src, key = key))

})


#### query ####
test_that("docdb_query", {

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

  # testJson
  expect_equal(docdb_create(src = src, key = key, value = testJson), 5L)
  expect_equal(dim(docdb_query(src = src, key = key, query = '{"friends.id": {"$gte": 0}}', limit = 1L)), c(1L, 11L))
  expect_error(docdb_query(src = src, key = key, query = '{"invalidjson'))
  #
  # testJson2
  expect_equal(docdb_create(src = src, key = key, value = testJson2), 2L)
  #
  expect_error(docdb_query(src = src, key = key, query = "NOTJSON"))
  expect_error(docdb_query(src = src, key = key, query = '{"$or": [{"$or": {"gear": 4}}, {"cyl": 6}]}')) # why error?
  expect_error(docdb_query(src = src, key = key, query = '{"gear": {"$unavailable": 6}}'))
  expect_warning(docdb_query(src = src, key = key, query = "", fields = '{"_id":1}'), "deprecated")
  expect_equal(dim(docdb_query(src = src, key = key, query = '{}')), c(7L, 15L))
  expect_equal(nrow(docdb_query(src = src, key = key, query = '{}', limit = 3L)), 3L)
  #
  expect_error(docdb_query(src = src, key = key, query = '{}', fields = "NOTJSON"))
  expect_equal(dim(docdb_query(src = src, key = key, query = '{}', fields = '{}')), c(7L, 15L))
  expect_equal(dim(docdb_query(src = src, key = key, query = '{}', fields = '{"_id": 1}')), c(7L, 1L))
  expect_equal(dim(docdb_query(src = src, key = key, query = '{}', fields = '{"email": 1}')), c(5L, 2L))
  #
  expect_equal(dim(docdb_query(src = src, key = key, query = '{"friends.id": 2}')), c(3L, 11L))
  expect_equal(dim(docdb_query(src = src, key = key, query = '{"friends.id": 2}', fields = '{"friends.id": 1}')), c(3L, 2L))
  expect_equal(dim(docdb_query(src = src, key = key, query = '{"friends.id": 2}', fields = '{"_id": 1}')), c(3L, 1L))
  expect_null(docdb_query(src = src, key = key, query = '{"friends.id": 9}', fields = '{"_id": 0, "notexisting": 1}'))
  #
  expect_equal(dim(docdb_query(src = src, key = key, query = '{"_id":"5cd67853f841025e65ce0ce2"}', fields = '{"email": 1, "_id": 0}')), c(1L, 1L))
  expect_equal(dim(docdb_query(src = src, key = key, query = '{"email": {"$regex": "lacychen@conjurica.com"}}')), c(1L, 11L))
  expect_equal(dim(docdb_query(src = src, key = key, query = '{"email": {"$regex": "^lacychen"}}')), c(1L, 11L))
  expect_equal(dim(docdb_query(src = src, key = key, query = '{"name": {"$ne": "Lacy Chen"}}')), c(4L, 11L))
  expect_equal(dim(docdb_query(src = src, key = key, query = '{"name": {"$regex": "^[a-zA-Z]{3,4} "}}', fields = '{"name": 1, "age": 1, "_id": 0}')), c(3L, 2L))
  #
  expect_equal(dim(docdb_query(src = src, key = key, query = '{"$and": [{"email": {"$regex": "ac"}}, {"friends.name": "Dona Bartlett"}]}')), c(1L, 11L))
  expect_equal(dim(docdb_query(src = src, key = key, query = '{"$and": [{"_id": {"$regex": "53"}}, {"friends.name": "Dona Bartlett"}]}')), c(1L, 11L))
  expect_equal(dim(docdb_query(src = src, key = key, query = '{"$and": [{"_id": {"$regex": "53"}}, {"friends.name": {"$regex": "Ba|La"}}]}')), c(2L, 11L))
  expect_equal(dim(docdb_query(src = src, key = key, query = '{"$and": [{"_id": {"$regex": "53"}}, {"friends.name": "Dona Bartlett"}]}')), c(1L, 11L))
  expect_equal(dim(docdb_query(src = src, key = key, query = '{"$or": [{"email": {"$regex": "ac"}}, {"friends.name": "Pace Bell"}]}')), c(3L, 11L))
  expect_true(docdb_query(src = src, key = key, query = '{"friends.name": "Dona Bartlett"}', fields = '{"name": 1}')[["name"]] == "Pace Bell")
  expect_equal(dim(docdb_query(src, key, query = '{"$or": [{"age": {"$gt": 21}}, {"friends.name": {"$regex": "^B[a-z]{3,9}.*"}}]}', fields = '{"age": 1, "friends.name": 1}')), c(3L, 3L))
  expect_equal(dim(docdb_query(src = src, key = key, query = '{"$or": [{"email": {"$regex": "^lacychen"}}, {"friends.name": "Dona Bartlett"}]}')), c(2L, 11L))
  expect_equal(dim(docdb_query(src = src, key = key, query = '{"$or": [{"email": {"$regex": "lacychen@conjurica.com"}}, {"tags": {"$regex": "^duis$"}}]}')), c(2L, 11L))
  expect_equal(dim(docdb_query(src = src, key = key, query = '{"friends.name": "Dona Bartlett"}', fields = '{"_id": 1}')), c(1L, 1L))
  #
  expect_equal(dim(docdb_query(src = src, key = key, query = '{"name": "Lacy Chen"}')), c(1L, 11L))
  expect_equal(dim(docdb_query(src = src, key = key, query = '{"age": 20}')), c(2L, 11L))
  expect_equal(dim(docdb_query(src = src, key = key, query = '{"age": 20}', fields = '{"name": 1, "age": 0, "_id": 1}')), c(2L, 2L))
  expect_equal(dim(docdb_query(src = src, key = key, query = '{"age": 20}', fields = '{"name": 0}')), c(2L, 10L))
  expect_equal(dim(docdb_query(src = src, key = key, query = '{"age": 20}', fields = '{"_id": 1, "friends": 1}')), c(2L, 2L))
  expect_equal(dim(docdb_query(src = src, key = key, query = '{"age": 20}', fields = '{"_id": 1, "friends.id": 1}')), c(2L, 2L))
  expect_equal(dim(docdb_query(src = src, key = key, query = '{"age": 20}', fields = '{"_id": 1, "friends": 1, "friends.id": 1}')), c(2L, 3L)) # 0.9.9 now can have overlapping paths
  expect_true(nrow(docdb_query(src = src, key = key, query = '{"age": 20}', fields = '{"_id": 1, "age": 1, "doesnotexist": 1}')) == 2L)
  expect_true(ncol(docdb_query(src = src, key = key, query = '{"age": 20}', fields = '{"_id": 1, "age": 1, "doesnotexist": 0}')) == 2L)
  expect_equal(dim(docdb_query(src = src, key = key, query = '{"email": "lacychen@conjurica.com"}')), c(1L, 11L))
  expect_equal(dim(docdb_query(src, key, query = '{"$or": [{"age": {"$lte": 20}}, {"age": {"$gte": 25}}]}')), c(3L, 11L))
  expect_equal(dim(docdb_query(src, key, query = '{"$or": [{"friends.id": {"$lt": 1}}, {"friends.id": {"$in": [2,3]}}]}')), c(5L, 11L))
  #
  empty <- docdb_query(src = src, key = key, query = '{"age": 20}', fields = '{"nonexistingfield": 1, "_id": 0}')
  expect_true(is.null(empty) || !ncol(empty) || !nrow(empty) || all(is.na(empty)) || names(empty) == "_id") # for 0.9.9 adding all is.na for duckdb
  expect_equal(dim(
    docdb_query(src = src, key = key, query = '{"tags": {"$regex": "^[a-z]{3,4}$"}}', fields = '{"name": 1, "age": 1, "_id": 0}')), c(3L, 2L))
  expect_true(docdb_delete(src = src, key = key))

  # special test for lifting nested values as unnested values
  expect_equal(docdb_create(src, key, value = '[{"a": {"b": {"c": 3}}},{"a": {"b": {"c": 4}}}]'), 2L)
  expect_equal(docdb_query(src = src, key = key, query = '{}', fields = '{"a.b.c": 1, "_id": 0}')[[1]], 3:4)
  tmp <- docdb_query(src = src, key = key, query = '{}', fields = '{"clinical_results.reported_events.other_events.category_list.category.event_list":1}')
  expect_true(is.null(tmp))
  if (any(inherits(src, "src_postgres"))) expect_error(docdb_query(src = src, key = key, query = '{}', fields = paste0('{"', paste0("a", 1:50, collapse = '":1,"'), '":1}')))
  expect_null(docdb_query(src = src, key = key, query = '{}', fields = paste0('{"', paste0("a", 1:49, collapse = '":1,"'), '":1}')))
  expect_true(docdb_delete(src = src, key = key))

  # testJson2
  expect_equal(docdb_create(src = src, key = key, value = testJson2), 2L)
  expect_equal(dim(docdb_query(src = src, key = key, query = '{}', fields = '{"rows.elements.distance.somevalue": 1}')), c(2L, 2L))
  expect_equal(nrow(docdb_query(src = src, key = key, query = '{}', fields = '{"destination_addresses": 1}')), 2L)
  expect_equal(length(unlist(docdb_query(
    src = src, key = key, query = '{"origin_addresses": {"$in": ["Santa Barbara, CA, USA"]}}', fields = '{"destination_addresses": 1, "_id": 0}'))), 3L)
  # note: str, typeof differ by database backend
  expect_true(docdb_delete(src = src, key = key))

  # testDf
  expect_equal(docdb_create(src = src, key = key, value = testDf), 32L)
  expect_equal(dim(docdb_query(src = src, key = key, query = '{}')), c(32L, 12L))
  expect_equal(dim(docdb_query(src = src, key = key, query = '{}', fields = '{"_id": 1}')), c(32L, 1L))
  expect_equal(dim(docdb_query(src, key, query = '{"gear":4, "mpg": {"$lte": 21.9}}')), c(5L, 12L))
  expect_equal(dim(docdb_query(src, key, query = '{"gear":4, "mpg": {"$lte": 21.9}}', fields = '{"_id": 1}')), c(5L, 1L))
  expect_equal(dim(docdb_query(src, key, query = '{"gear": {"$in": [5,4]}}')), c(17L, 12L))
  expect_equal(dim(docdb_query(src, key, query = '{"_id": {"$in": ["Datsun 710", "Merc 280C"]}}')), c(2L, 12L))
  expect_equal(dim(docdb_query(src = src, key = key, query = '{"mpg": {"$lte": 18.9}}', fields = '{"mpg": 1, "carb": 1}')), c(15L, 3L)) # mpg is not integer
  expect_equal(sum(docdb_query(src = src, key = key, query = '{"disp": {"$gt": 350}}', fields = '{"carb": 1, "_id": 0}')[[1]]), 24L)
  expect_identical(docdb_query(src = src, key = key, query = '{"$and": [{"mpg": {"$lte": 18}}, {"gear": {"$gt": 3}}]}'),
                   docdb_query(src = src, key = key, query = '          {"mpg": {"$lte": 18},   "gear": {"$gt": 3}}'))
  expect_equal(nrow(docdb_query(src = src, key = key, query = '{"$or": [{"cyl": {"$lte": 5}}, {"_id": {"$regex": "^F[a-z].*", "$options": ""}}]}')), 13L)
  # ^^^ 0.9.9 changed to test on integer column since duckdb json may not have decimals

  # listfields
  expect_equal(docdb_create(src = src, key = key, value = testJson), 5L)
  fields <- docdb_query(src = src, key = key, query = '{}', listfields = TRUE)
  expect_true(length(fields) == 23)
  fields <- docdb_query(src = src, key = key, query = '{}', listfields = TRUE, limit = 999L)
  expect_true(length(fields) == 23)
  fields <- docdb_query(src = src, key = key, query = '{"name": {"$regex": "^[LW].*"}}', listfields = TRUE)
  expect_true(length(fields) == 12)
  fields <- docdb_query(src = src, key = key, query = '{"age": 20}', listfields = TRUE, limit = 2L)
  expect_true(length(fields) == 12) # query for top level element does not require jq query
  fields <- docdb_query(src = src, key = key, query = '{"name": {"$regex": "^[LW].*"}}', listfields = TRUE, limit = 2L)
  expect_true(length(fields) == 12)
  fields <- docdb_query(src = src, key = key, query = '{"friends.name": {"$regex": "^[LW].*"}}', listfields = TRUE, limit = 2L)
  expect_true(length(fields) == 12)

  # clean up
  expect_true(docdb_delete(src = src, key = key))

})

#### update ####
test_that("docdb_update", {

  # get db connection
  tmp <- dbSrcKey()
  src <- tmp$testSrc
  key <- tmp$testKey
  tF <- testFile()
  on.exit(try({
    docdb_delete(src = src, key = key)
    if (any(c(inherits(src, "src_sqlite"), inherits(src, "src_postgres")))) DBI::dbDisconnect(src$con, shutdown = TRUE)
    if (inherits(src, "src_duckdb")) duckdb::dbDisconnect(src$con, shutdown = TRUE)
    rm(src, key, tmp, tF)
  }, silent = TRUE), add = TRUE)

  expect_equal(docdb_create(src = src, key = key, value = testDf), nrow(testDf))

  # tests0
  expect_equal(docdb_update(src = src, key = key, value = '{"vs": 77}', query = '{"gear": {"$in": [5,4]}}'), 17L)
  expect_true(all(docdb_query(src, key, query = '{"gear": {"$in": [5,4]}}', fields = '{"vs": 1}')[["vs"]] == 77L))
  expect_equal(sum(docdb_get(src = src, key = key)[["_id"]] == "Cadillac Fleetwood"), 1L)

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

  # tests5 bulk updates
  expect_equal(docdb_update(src = src, key = key, value = '{"vs": 99}', query = '{"gear": 5}'), 5L)
  expect_equal(docdb_update(src = src, key = key, value = '{"_id":"Valiant", "vs": 99}', query = '{}'), 1L)
  expect_equal(sort(docdb_query(src = src, key = key, query = '{"vs": 99}', fields = '{"gear":1}')[["gear"]]), c(3,5,5,5,5,5))
  #
  expect_equal(docdb_update(src = src, key = key, value = list("_id" = "Valiant", "gear" = 8), query = "{}"), 1L)
  expect_equal(docdb_update(src = src, key = key, value = list(list("_id" = "Valiant", "gear" = 8), list("_id" = "Fiat 128", "gear" = 9)), query = '{}'), 2L)
  expect_equal(docdb_update(src = src, key = key, value = data.frame("_id" = c("Valiant", "Fiat 128"), "gear" = 8:9, check.names = FALSE, stringsAsFactors = FALSE), query = '{}'), 2L)
  expect_equal(docdb_update(src = src, key = key, value = data.frame("_id" = c("Valiant", "Fiat 128"), "gear" = 8:9, check.names = FALSE, stringsAsFactors = FALSE)[1,], query = '{}'), 1L)
  expect_equal(docdb_update(src = src, key = key, value = '[{"_id":"Valiant", "vs": 77},{"_id":"Fiat 128", "vs": 78}]', query = '{}'), 2L)
  expect_equal(docdb_update(src = src, key = key, value = c('{"_id":"Valiant", "vs": 78}','{"_id":"Fiat 128", "vs": 79}'), query = '{}'), 2L)
  #
  expect_equal(docdb_update(src = src, key = key, value = '{"q":[{"t":1,"r":"C"},{"s":1,"r":"D"}]}', query = '{"_id":"Fiat 128"}'), 1L)
  tmp <- docdb_query(src = src, key = key, query = '{"_id":"Fiat 128"}', fields = '{"q":1}')[["q"]][[1]]
  expect_equal(tmp[, c("t", "r", "s")], jsonlite::fromJSON('[{"t":1,"r":"C"},{"s":1,"r":"D"}]'))
  #
  expect_true(docdb_update(src = src, key = key, value = list("_id" = c("Valiant", "Fiat 128"), "gear" = 8:9), query = '{}') %in% c(0L, 2L))
  expect_error(docdb_update(src = src, key = key, value = '{"_id":["Valiant","Fiat 128"], "vs": [77,77]}', query = '{}'), "class character|array of documents")
  expect_warning(docdb_update(src = src, key = key, value = '{"_id":"Valiant", "vs": 79}', query = '{"_id": "a"}'), "gnoring.*query")
  expect_equal(docdb_query(src = src, key = key, query = '{"gear":3}', fields = '{"hp":1}')[["hp"]][c(1,14)], list(c(110,110), c(110,110)))
  expect_equal(docdb_query(src = src, key = key, query = '{"_id":"Valiant"}', fields = '{"vs":1}')[["vs"]], 79L)
  #
  # from file
  expect_equal(docdb_create(src = src, key = key, value = tF), 5L)
  expect_equal(docdb_update(src = src, key = key, value = tF, query = '{}'), 5L)
  expect_equal(docdb_update(src = src, key = key, value = jqr::jq(file(tF), " del ( ._id) "), query = '{"email": {"$regex": ".+"}}'), 5L)

  # warnings and errors
  expect_warning(docdb_update(src = src, key = key, value = testJson, query = ""), "deprecated")
  expect_warning(docdb_update(src = src, key = key, value = tF, query = '{"_id": {"$regex": "[f-z]"}}'), "Ignoring the specified")
  expect_error(docdb_update(src = src, key = key, value = testJson2, query = '{"_id": {"$regex": "[f-z]"}}'), "Unequal number of documents")

  # from url
  skip_if(is.null(httpbin), "package webfakes missing")
  #
  expect_error(docdb_update(src = src, key = key, value = httpbin$url("/stream/3"), query = '{"gear": 4}'), "Unequal number")
  expect_equal(docdb_update(src = src, key = key, value = httpbin$url("/stream/14"), query = '{"gear": 3}'), 14L)
  expect_equal(sort(names(docdb_query(src = src, key = key, query = '{"_id":"Hornet 4 Drive"}', fields = '{"args":0, "id":0}'))), # Elastic id issue
               sort(c("_id","mpg","cyl","disp","hp","drat","wt","qsec","vs","am","gear","carb","url","headers","origin")))

})

#### transactions ####
test_that("parallel writes", {

  # get db connection
  tmp <- dbSrcKey()
  src <- tmp$testSrc
  cls <- class(src)[1]

  if (any(cls == c("src_elastic", "src_couchdb", "src_mongo", "src_duckdb"))) skip(
    "Testing for parallel writes not possible or implemented")

  if (any(cls == c("src_sqlite", "src_postgres"))) {
    DBI::dbDisconnect(src$con, shutdown = TRUE)
  }

  ## sqlite
  if (cls == "src_sqlite") {

    tfF <- function(..., env = parent.frame()) {
      tfF <- tempfile(fileext = ".sqlite")
      withr::defer(try(unlink(tfF), silent = TRUE), envir = env)
      tfF
    }

    key <- createKey()
    tf <- tfF()
    #
    #
    rp1 <- callr::r_bg(function(tf, key) {
      src <- nodbi::src_sqlite(dbname = tf, key = key)
      nodbi::docdb_create(src = src, key = key, value = iris)
    }, args = list(tf, key))
    rp2 <- callr::r_bg(function(tf, key) {
      src <- nodbi::src_sqlite(dbname = tf, key = key)
      nodbi::docdb_create(src = src, key = key, value = iris)
    }, args = list(tf, key))
    #
    rp1$wait(); rp2$wait()
    #
    expect_equal(rp1$get_result() + rp2$get_result(), 2 * nrow(iris))
    src <- src_sqlite(dbname = tf, key = key)
    expect_equal(dim(docdb_get(src = src, key = key)), c(300L, 6L))
    #
    #
    expect_true(docdb_delete(src = src, key = key))
    DBI::dbDisconnect(src$con, shutdown = TRUE)
    unlink(tf)

  }

  ## postgresql
  if (cls == "src_postgres") {

    key <- createKey()
    #
    #
    rp1 <- callr::r_bg(function(key) {
      src <- nodbi::src_postgres()
      nodbi::docdb_create(src = src, key = key, value = iris)
    }, args = list(key))
    rp2 <- callr::r_bg(function(key) {
      src <- nodbi::src_postgres()
      nodbi::docdb_create(src = src, key = key, value = iris)
    }, args = list(key))
    #
    rp1$wait(); rp2$wait()
    #
    expect_equal(rp1$get_result() + rp2$get_result(), 2 * nrow(iris))
    src <- nodbi::src_postgres()
    expect_equal(dim(docdb_get(src = src, key = key)), c(300L, 6L))
    #
    #
    expect_true(docdb_delete(src = src, key = key))
    DBI::dbDisconnect(src$con, shutdown = TRUE)

  }

  ## duckdb
  #
  # https://duckdb.org/docs/api/r.html
  # Read-only mode is required if multiple R processes
  # want to access the same database file at the same time.

})


#### general ####
test_that("auto disconnect and shut down", {

  suppressWarnings({

    # get db connection
    tmp1 <- dbSrcKey(); tmp1 <- tmp1$testSrc
    tmp2 <- dbSrcKey(); tmp2 <- tmp2$testSrc

    cls <- class(tmp1)[1]

  })

  if (any(cls == c("src_elastic", "src_couchdb", "src_mongo"))) skip(
    "Testing for auto disconnect and shutdown not relevant")

  suppressWarnings({

    assign("tmp1", tmp1, envir = .GlobalEnv)
    assign("tmp2", tmp2, envir = .GlobalEnv)

    on.exit(try(suppressWarnings(
      DBI::dbDisconnect(tmp1$con, shutdown = TRUE)),
      silent = TRUE), add = TRUE)

    on.exit(try(suppressWarnings(
      DBI::dbDisconnect(tmp2$con, shutdown = TRUE)),
      silent = TRUE), add = TRUE)

    on.exit(try(
      rm(tmp1, tmp2, envir = .GlobalEnv),
      silent = TRUE), add = TRUE)

  })

  # test if applicable
  expect_message(
    nodbi:::.onUnload(),
    "nodbi: docdb_src 'tmp.' disconnected and shut down.")

})

test_that("internal functions", {

  expect_error(assert(iris, c("character", "integer")))

  .nodbi <- new.env()
  expect_null(nodbi:::initTransformers())

  expect_null(nodbi:::.onLoad())

  expect_error(nodbi:::insObj("non-atomics like data frame /** iris **/ fail"))

  tmp <- "xyz"
  expect_equal(nodbi:::insObj("with /** 'tmp' **/ brackets"), "with 'xyz' brackets")

})

