#' Update documents
#'
#' @export
#' @param src source object, result of call to src
#' @param key A key. See Details.
#' @param value A value
#' @param ... Ignored
#' @details Only CouchDB supported for now
#' @examples \dontrun{
#' # CouchDB
#' src <- src_couchdb()
#' docdb_create(src, "mtcars2", mtcars)
#' docdb_get(src, "mtcars2")
#'
#' mtcars$letter <- sample(letters, NROW(mtcars), replace = TRUE)
#' docdb_update(src, "mtcars2", mtcars)
#' docdb_get(src, "mtcars2")
#' }
docdb_update <- function(src, key, value, ...) {
  UseMethod("docdb_update")
}

#' @export
docdb_update.src_couchdb <- function(src, key, value, ...) {
  if (!key %in% attr(src, "dbs")) sofa::db_create(src[[1]], dbname = key)
  sofa::db_bulk_update(src[[1]], dbname = key, doc = value, ...)
}
