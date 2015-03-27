#' Get documents
#'
#' @export
#' @param conn Connection object, result of call to src
#' @param docid Document ID
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
#' conn <- src_couchdb()
#' docout <- docdb_create(conn, mtcars)
#' docdb_get(conn, pluck(docout, "id", "")[1])
#' docdb_get(conn, pluck(docout, "id", "")[1:5])
#' docdb_get(conn, pluck(docout, "id", ""))
#' }
docdb_get <- function(src, docid, ...){
  UseMethod("docdb_get")
}

#' @export
docdb_get.src_couchdb <- function(src, docid, ...){
  tmp <- lapply(docid, function(z) doc_get(cushion = src$type, dbname = attr(src, "dbname"), docid = z))
  df <- rbindlist(unname(tmp))
  dropmeta(df)
}

dropmeta <- function(x) {
  x$`_id` <- NULL
  x$`_rev` <- NULL
  x
}

# to_df <- function(x) {
#   x$`_id` <- NULL
#   x$`_rev` <- NULL
#   x
# #   as.data.frame(rbind(x), stringsAsFactors = FALSE)
# }
