#' Get documents
#'
#' @export
#' @import data.table jsonlite
#' @param src source object, result of call to src
#' @param docid Document ID
#' @param ... Ignored for now
#' @examples \dontrun{
#' # CouchDB
#' src <- src_couchdb()
#' docout <- docdb_create(src, key = "mtcars2", value = mtcars)
#' docdb_get(src, "mtcars2")
#'
#' # Etcd
#' # src <- src_etcd()
#' # docdb_create(src, "/hello", mtcars)
#' # docdb_get(src, "/hello")
#'
#' # Elasticsearch
#' src <- src_elasticsearch()
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
#'
#' # Riak
#' src <- src_riak()
#' docdb_create(src, "mtcars", mtcars)
#' docdb_get(src, docid="mtcars")
#' }
docdb_get <- function(src, docid, ...){
  UseMethod("docdb_get")
}

#' @export
docdb_get.src_couchdb <- function(src, docid, ...) {
  dropmeta(makedf(
    pluck(sofa::db_alldocs(src[[1]], dbname = docid,
                           include_docs = TRUE, ...)$rows, "doc")))
}

#' @export
docdb_get.src_etcd <- function(src, docid, ...) {
  tmp <- src$key(docid, recursive = TRUE, sorted = TRUE, ...)
  makedf(
    lapply(pluck(tmp$node$nodes, "value"), jsonlite::fromJSON)
  )
}

#' @export
docdb_get.src_elasticsearch <- function(src, docid, ...){
  ids <- pluck(elastic::Search(docid, source = FALSE,
                               size = 1000)$hits$hits, "_id", "")
  tmp <- elastic::docs_mget(index = docid, type = docid, ids = ids,
                            verbose = FALSE)
  makedf(pluck(tmp$docs, "_source"))
}

#' @export
docdb_get.src_redis <- function(src, docid, ...) {
  res <- src$con$GET(docid)
  if (is.null(res)) stop("no matching result found")
  RedisAPI::string_to_object(res)
}

#' @export
docdb_get.src_mongo <- function(src, docid, ...) {
  dump <- tempfile()
  src$con$export(file(dump))
  # remove first column, a mongodb identifier
  jsonlite::stream_in(file(dump), verbose = FALSE)[,-1]
}

#' @export
docdb_get.src_riak <- function(src, docid, ...) {
  reeack::riak_unserialize(src$con$fetch(key = docid))
}

dropmeta <- function(x) {
  x$`_id` <- NULL
  x$`_rev` <- NULL
  x
}
