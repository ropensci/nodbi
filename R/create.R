#' Read documents.
#'
#' @export
#' @examples \dontrun{
#' conn <- src_couchdb()
#' library("jsonlite")
#' doc <- fromJSON("http://api.gbif.org/v1/species/2704179")
#' db_create(conn, doc)
#' }
db_create <- function(conn, doc){
  doc_create(doc = doc, cushion = conn$type, dbname = attr(conn, "dbname"))
}
