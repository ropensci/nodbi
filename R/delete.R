#' Delete documents
#'
#' @export
#' @param src source object, result of call to src
#' @param key A key
#' @param ... Ignored for now
#' @examples \dontrun{
#' # etcd
#' src <- src_etcd()
#' docdb_create(src, "/iris", iris)
#' docdb_get(src, "/iris")
#' docdb_delete(src, "/iris")
#' }
docdb_delete <- function(src, key, ...){
  UseMethod("docdb_delete")
}

# #' @export
# docdb_delete.src_couchdb <- function(src, key, ...){
#   dropmeta(rbindlist(pluck(sofa::alldocs(cushion = src$type, dbname = docid, include_docs = TRUE)$rows, "doc")))
# }

#' @export
docdb_delete.src_etcd <- function(src, key, ...){
  etseed::delete(key, dir = TRUE, recursive = TRUE)
}
