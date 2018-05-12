#' Get documents
#'
#' @export
#' @import data.table jsonlite
#' @param src source object, result of call to src
#' @param key (chartacter) A key. ignored for mongo
#' @param ... Ignored for now
#' @template deets
#' @examples \dontrun{
#' # CouchDB
#' src <- src_couchdb()
#' docout <- docdb_create(src, key = "mtcars2", value = mtcars)
#' docdb_get(src, "mtcars2")
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
#' }
docdb_get <- function(src, key, ...){
  UseMethod("docdb_get")
}

#' @export
docdb_get.src_couchdb <- function(src, key, ...) {
  assert(key, 'character')
  dropmeta(makedf(
    pluck(sofa::db_alldocs(src$con, dbname = key,
                           include_docs = TRUE, ...)$rows, "doc")))
}

#' @export
docdb_get.src_etcd <- function(src, key, ...) {
  assert(key, 'character')
  tmp <- src$key(key, recursive = TRUE, sorted = TRUE, ...)
  makedf(
    lapply(pluck(tmp$node$nodes, "value"), jsonlite::fromJSON)
  )
}

#' @export
docdb_get.src_elastic <- function(src, key, ...){
  assert(key, 'character')
  ids <- pluck(elastic::Search(key, source = FALSE,
                               size = 1000)$hits$hits, "_id", "")
  tmp <- elastic::docs_mget(index = key, type = key, ids = ids,
                            verbose = FALSE)
  makedf(pluck(tmp$docs, "_source"))
}

#' @export
docdb_get.src_redis <- function(src, key, ...) {
  assert(key, 'character')
  res <- src$con$GET(key)
  if (is.null(res)) stop("no matching result found")
  redux::string_to_object(res)
}

#' @export
docdb_get.src_mongo <- function(src, key, ...) {
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
