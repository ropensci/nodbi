#' Update documents
#'
#' @export
#' @param src source object, result of call to src
#' @param key A key. See Details.
#' @param value A value
#' @param ... Ignored
#' @details Note that with etcd, you have to prefix a key with a forward slash.
#' @examples \dontrun{
#' # CouchDB
#' src <- src_couchdb()
#' docdb_create(src, "mtcars3", mtcars)
#' docdb_get(src, "mtcars3")
#'
#' mtcars$letter <- sample(letters, NROW(mtcars), replace = TRUE)
#' docdb_update(src, "mtcars3", mtcars)
#' docdb_get(src, "mtcars3")
#' }
docdb_update <- function(src, key, value, ...){
  UseMethod("docdb_update")
}

#' @export
docdb_update.src_couchdb <- function(src, key, value, ...){
  dbinfo <- sofa::db_info(dbname = key)
  if (!is.null(dbinfo$error)) sofa::db_create(dbname = key)
  sofa::bulk_update(doc = value, cushion = src$type, dbname = key)
}
