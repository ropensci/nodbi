#' Create documents
#'
#' @export
#' @param conn Connection object, result of call to src
#' @param value Document
#' @examples \dontrun{
#' conn <- src_couchdb()
#' library("jsonlite")
#' doc <- fromJSON("http://api.gbif.org/v1/species/2704179")
#' docdb_create(conn, doc)
#'
#' doc2 <- fromJSON("http://api.gbif.org/v1/species/2704174")
#' docdb_create(conn, doc2)
#' }
docdb_create <- function(src, value, ...){
  UseMethod("docdb_create")
}

#' @export
docdb_create.src_couchdb <- function(src, value, ...){
  doc_create(doc = value, cushion = src$type, dbname = attr(src, "dbname"))
}
