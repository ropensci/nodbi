#' Get documents
#'
#' @export
#' @import data.table
#' @param src source object, result of call to src
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
#' src <- src_couchdb()
#' docout <- docdb_create(src, key = "mtcars", value = mtcars)
#' docdb_get(src, "mtcars")
#'
#' docdb_get(conn, pluck(docout, "id", "")[1])
#' docdb_get(conn, pluck(docout, "id", "")[1:5])
#' docdb_get(conn, pluck(docout, "id", ""))
#' }
docdb_get <- function(src, docid, ...){
  UseMethod("docdb_get")
}

#' @export
docdb_get.src_couchdb <- function(src, docid, ...){
  dropmeta(rbindlist(pluck(alldocs(cushion = src$type, dbname = docid, include_docs = TRUE)$rows, "doc")))
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
