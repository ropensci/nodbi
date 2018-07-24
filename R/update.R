#' Update documents
#'
#' @export
#' @param src source object, result of call to src
#' @param key (chartacter) A key. ignored for mongo
#' @param value (data.frame) A single data.frame
#' @param ... Ignored
#' @details Only CouchDB supported for now
#' @examples \dontrun{
#' # CouchDB
#' src <- src_couchdb()
#' docdb_create(src, "mtcars2", mtcars)
#' docdb_get(src, "mtcars2")
#'
#' mtcars$letter <- sample(letters, NROW(mtcars), replace = TRUE)
#' invisible(docdb_update(src, "mtcars2", mtcars))
#' docdb_get(src, "mtcars2")
#' }
docdb_update <- function(src, key, value, ...) {
  UseMethod("docdb_update")
}

#' @export
docdb_update.src_couchdb <- function(src, key, value, ...) {
  assert(value, 'data.frame')
  if (!key %in% attr(src, "dbs")) sofa::db_create(src[[1]], dbname = key)
  sofa::db_bulk_update(src[[1]], dbname = key, doc = value, ...)
}
