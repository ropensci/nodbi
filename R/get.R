#' Get documents
#'
#' @export
#' @import data.table jsonlite
#' @param src source object, result of call to src
#' @param docid Document ID
#' @param ... Ignored for now
#' @examples \dontrun{
#' conn <- src_couchdb()
#' library("jsonlite")
#' doc <- fromJSON("http://api.gbif.org/v1/species/2704179")
#' res <- docdb_create(conn, doc)
#' docdb_get(conn, res$id)
#'
#' doc2 <- fromJSON("http://api.gbif.org/v1/species/2704174")
#' docout <- docdb_create(conn, doc2)
#' docdb_get(conn, docout$id)
#'
#' # entire data.frame
#' src <- src_couchdb()
#' docout <- docdb_create(src, key = "mtcars", value = mtcars)
#' docdb_get(src, "mtcars")
#'
#' docdb_get(conn, pluck(docout, "id", "")[1])
#' docdb_get(conn, pluck(docout, "id", "")[1:5])
#' docdb_get(conn, pluck(docout, "id", ""))
#'
#' # etcd
#' src <- src_etcd()
#' docdb_create(src, "/hello", "world")
#' docdb_get(src, "/hello")
#'
#' # Elasticsearch
#' src <- src_elasticsearch()
#' docdb_create(src, "iris", iris)
#' docdb_get(src, "iris")
#' }
docdb_get <- function(src, docid, ...){
  UseMethod("docdb_get")
}

#' @export
docdb_get.src_couchdb <- function(src, docid, ...){
  dropmeta(rbindlist(pluck(sofa::alldocs(cushion = src$type, dbname = docid, include_docs = TRUE)$rows, "doc")))
}

#' @export
docdb_get.src_etcd <- function(src, docid, ...){
  tmp <- etseed::key(docid, recursive = TRUE)
  rbindlist(
    lapply(pluck(tmp$node$nodes, "value"), jsonlite::fromJSON)
  )
}

#' @export
docdb_get.src_elasticsearch <- function(src, docid, ...){
  ids <- pluck(elastic::Search(docid, source = FALSE, size = 1000)$hits$hits, "_id", "")
  tmp <- elastic::docs_mget(index = docid, type = docid, ids = ids)
  rbindlist(pluck(tmp$docs, "_source"))
}

#' @export
docdb_get.src_rrlite <- function(src, docid, ...) {
  rrlite::from_redis(docid, src$con, ...)
}

dropmeta <- function(x) {
  x$`_id` <- NULL
  x$`_rev` <- NULL
  x
}
