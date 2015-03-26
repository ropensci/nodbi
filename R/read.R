#' Get documents
#'
#' @export
#' @examples \dontrun{
#' conn <- src_couchdb()
#' library("jsonlite")
#' doc <- fromJSON("http://api.gbif.org/v1/species/2704179")
#' res <- docdb_create(conn, doc)
#' docdb_get(conn, res$id)
#' }
docdb_get <- function(conn, docid){
  to_df(doc_get(cushion = conn$type, dbname = attr(conn, "dbname"), docid = docid))
}

to_df <- function(x) {
  as.data.frame(rbind(x), stringsAsFactors = FALSE)
}
