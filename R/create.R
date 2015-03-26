#' Create a document
#'
#' @export
#' @param conn Connection object, result of call to src
#' @param docid Document ID
#' @examples \dontrun{
#' conn <- src_couchdb()
#' library("jsonlite")
#' doc <- fromJSON("http://api.gbif.org/v1/species/2704179")
#' docdb_create(conn, doc)
#'
#' doc2 <- fromJSON("http://api.gbif.org/v1/species/2704174")
#' docdb_create(conn, doc2)
#' }
docdb_create <- function(conn, docid){
  UseMethod("docdb_create")
}

#' @export
docdb_create.src_couchdb <- function(conn, docid){
  doc_create(doc = docid, cushion = conn$type, dbname = attr(conn, "dbname"))
}
