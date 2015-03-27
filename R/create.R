#' Create documents
#'
#' @export
#' @param src source object, result of call to src
#' @param key A key
#' @param value A value
#' @param ... Ignored for now
#' @examples \dontrun{
#' conn <- src_couchdb()
#' library("jsonlite")
#' doc <- fromJSON("http://api.gbif.org/v1/species/2704179")
#' docdb_create(conn, doc)
#'
#' doc2 <- fromJSON("http://api.gbif.org/v1/species/2704174")
#' docdb_create(conn, doc2)
#'
#' key="mtcars2"
#' value=mtcars
#' docdb_create(src, key, value)
#' docdb_get(src, key)
#'
#' # etcd
#' src <- src_etcd()
#' docdb_create(src, "hello", "world")
#' }
docdb_create <- function(src, key, value, ...){
  UseMethod("docdb_create")
}

#' @export
docdb_create.src_couchdb <- function(src, key, value, ...){
  dbinfo <- sofa::db_info(dbname = key)
  if(!is.null(dbinfo$error)) sofa::db_create(dbname=key)
  sofa::bulk_create(doc = value, cushion = src$type, dbname = key)
}

#' @export
docdb_create.src_etcd <- function(src, key, value, ...){
  etseed::create(key = key, value = value, ...)
}
