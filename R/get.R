#' Get documents
#'
#' @export
#' @import data.table jsonlite
#' @param src source object, result of call to src
#' @param key (character) A key (collection for mongo)
#' @param limit (integer) number of records/rows to return. by default
#' not passed, so you get all results. Only works for CouchDB, 
#' Elasticsearch and MongoDB; ignored for others
#' @param ... passed on to functions:
#' 
#' - CouchDB: passed to [sofa::db_alldocs()]
#' - Elasticsearch: passed to [elastic::Search()]
#' - Redis: ignored
#' - MongoDB: ignored
#' - SQLite: ignored
#' 
#' @template deets
#' @examples \dontrun{
#' # CouchDB
#' src <- src_couchdb()
#' docout <- docdb_create(src, key = "mtcars2", value = mtcars)
#' docdb_get(src, "mtcars2")
#' docdb_get(src, "mtcars2", limit = 5)
#'
#' # Elasticsearch
#' src <- src_elastic()
#' if (docdb_exists(src, "iris")) docdb_delete(src, "iris")
#' docdb_create(src, "iris", iris)
#' Sys.sleep(2)
#' docdb_get(src, "iris")
#' if (docdb_exists(src, "d2")) docdb_delete(src, "d2")
#' docdb_create(src, "d2", diamonds)
#' Sys.sleep(3)
#' docdb_get(src, "d2", limit = 1010)
#'
#' # Redis
#' src <- src_redis()
#' docdb_create(src, "mtcars", mtcars)
#' docdb_get(src, "mtcars")
#'
#' # Mongo
#' src <- src_mongo(collection = "mtcars")
#' docdb_create(src, "mtcars", mtcars)
#' docdb_get(src, "mtcars")
#' docdb_get(src, "mtcars", limit = 4)
#' 
#' # SQLite
#' src <- src_sqlite()
#' docdb_create(src, "mtcars", mtcars)
#' docdb_get(src, "mtcars")
#' docdb_get(src, "mtcars", limit = 4L)
#' }
docdb_get <- function(src, key, limit = NULL, ...){
  UseMethod("docdb_get")
}

#' @export
docdb_get.src_couchdb <- function(src, key, limit = NULL, ...) {
  assert(key, 'character')
  dropmeta(makedf(
    pluck(sofa::db_alldocs(src$con, dbname = key,
                           include_docs = TRUE, limit = limit, ...)$rows, "doc")))
}

#' @export
docdb_get.src_elastic <- function(src, key, limit = NULL, ...){
  assert(key, 'character')
  ids <- pluck(elastic::Search(src$con, key, source = FALSE,
                               size = limit, ...)$hits$hits, "_id", "")
  tmp <- elastic::docs_mget(src$con, index = key, type = key, ids = ids,
    verbose = FALSE)
  makedf(pluck(tmp$docs, "_source"))
}

#' @export
docdb_get.src_redis <- function(src, key, limit = NULL, ...) {
  assert(key, 'character')
  res <- src$con$GET(key)
  if (is.null(res)) stop("no matching result found")
  redux::string_to_object(res)
}

#' @export
docdb_get.src_mongo <- function(src, key, limit = NULL, ...) {
  
  # check expectations
  if (exists("key", inherits = FALSE) && 
      src$collection != key) 
    message("Parameter 'key' is different from parameter 'collection', ",
            "was given as ", src$collection, " in src_mongo().")
  
  # FIXME: or use $find() here? not if doing a separate query method
  if (!is.null(limit)) return(src$con$iterate(limit = limit)$page())
  dump <- tempfile()
  src$con$export(file(dump))
  # remove first column, a mongodb identifier
  jsonlite::stream_in(file(dump), verbose = FALSE) # [,-1]
}

#' @export
docdb_get.src_sqlite <- function(src, key, limit = NULL, ...) {
  
  assert(key, "character")
  assert(limit, "integer")
  
  # arguments for call
  statement <- paste0(
    # _id is included into json for use with jsonlite::stream_in
    "SELECT '{\"_id\": \"' || _id || '\", ' || substr(json, 2) ", 
    "FROM ", key, " ;")
  
  # set limit if not null
  n <- -1L
  if (!is.null(limit)) n <- limit
  
  # temporary file for streaming
  tfname <- tempfile()
  dump <- file(description = tfname,
               encoding = "UTF-8")
  
  # register to remove file
  # after used for streaming
  on.exit(unlink(tfname))
  
  # get data, write to file in ndjson format
  cat(stats::na.omit(unlist(
    DBI::dbGetQuery(conn = src$con,
                    statement = statement, 
                    n = n))
  ),
  sep = "\n", # ndjson
  file = dump)
  
  # from jsonlite documentation:
  # Because parsing huge JSON strings is difficult and inefficient, 
  # JSON streaming is done using lines of minified JSON records, a.k.a. ndjson. 
  jsonlite::stream_in(dump, verbose = FALSE)

}

## helpers --------------------------------------

dropmeta <- function(x) {
  x$`_id` <- NULL
  x$`_rev` <- NULL
  x
}
