#' Create a document
#'
#' @export
#' @examples \dontrun{
#' conn <- src_couchdb()
#' library("jsonlite")
#' doc <- fromJSON("http://api.gbif.org/v1/species/2704179")
#' docdb_create(conn, doc)
#'
#' doc2 <- fromJSON("http://api.gbif.org/v1/species/2704174")
#' docdb_create(conn, doc2)
#' }
docdb_create <- function(conn, doc){
  UseMethod("docdb_create")
}

#' @export
docdb_create.src_couchdb <- function(conn, doc){
  doc_create(doc = doc, cushion = conn$type, dbname = attr(conn, "dbname"))
}
