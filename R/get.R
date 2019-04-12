#' Get documents
#'
#' @export
#' @import data.table jsonlite
#' @param src source object, result of call to src
#' @param key (chartacter) A key. ignored for mongo
#' @param limit (integer) number of records/rows to return. by default
#' not passed, so you get all results. Only works for CouchDB, 
#' Elasticsearch and MongoDB; ignored for others
#' @param ... passed on to functions:
#' 
#' - CouchDB: passed to [sofa::db_alldocs()]
#' - etcd: passed to the `$key()` method
#' - Elasticsearch: passed to [elastic::Search()]
#' - Redis: ignored
#' - MongoDB: ignored
#' 
#' @template deets
#' @examples \dontrun{
#' # CouchDB
#' src <- src_couchdb()
#' docout <- docdb_create(src, key = "mtcars2", value = mtcars)
#' docdb_get(src, "mtcars2")
#' docdb_get(src, "mtcars2", limit = 5)
#'
#' # etcd
#' # src <- src_etcd()
#' # docdb_create(src, "/hello", mtcars)
#' # docdb_get(src, "/hello")
#'
#' # Elasticsearch
#' src <- src_elastic()
#' docdb_create(src, "iris", iris)
#' docdb_get(src, "iris")
#' docdb_create(src, "d2", diamonds)
#' docdb_get(src, "d2", limit = 1010)
#'
#' # Redis
#' src <- src_redis()
#' docdb_create(src, "mtcars", mtcars)
#' docdb_get(src, "mtcars")
#'
#' # Mongo
#' src <- src_mongo()
#' docdb_create(src, "mtcars", mtcars)
#' docdb_get(src, "mtcars")
#' docdb_get(src, "mtcars", limit = 4)
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
docdb_get.src_etcd <- function(src, key, limit = NULL, ...) {
  assert(key, 'character')
  tmp <- src$key(key, recursive = TRUE, sorted = TRUE, ...)
  makedf(
    lapply(pluck(tmp$node$nodes, "value"), jsonlite::fromJSON)
  )
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
  # FIXME: or use $find() here? not if doing a separate query method
  if (!is.null(limit)) return(src$con$iterate(limit = limit)$page())
  dump <- tempfile()
  src$con$export(file(dump))
  # remove first column, a mongodb identifier
  jsonlite::stream_in(file(dump), verbose = FALSE)[,-1]
}

dropmeta <- function(x) {
  x$`_id` <- NULL
  x$`_rev` <- NULL
  x
}
