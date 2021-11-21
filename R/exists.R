#' Check if container exists in database
#'
#' @inheritParams docdb_create
#'
#' @param ... Passed to functions:
#' - MongoDB: find() in [mongolite::mongo()]
#' - RSQLite: [DBI::dbListTables()]
#' - Elasticsearch: [elastic::index_exists()]
#' - CouchDB: [sofa::db_info()]
#'
#' @return (logical) `TRUE` or `FALSE` to indicate
#'  existence of container \code{key} in database
#'
#' @export
#'
#' @examples \dontrun{
#' src <- src_sqlite()
#' docdb_exists(src, "nonexistingcontainer")
#' docdb_create(src, "mtcars", mtcars)
#' docdb_exists(src, "mtcars")
#' }
#'
docdb_exists <- function(src, key, ...) {
  assert(src, "docdb_src")
  assert(key, "character")
  UseMethod("docdb_exists", src)
}

#' @export
docdb_exists.src_couchdb <- function(src, key, ...) {
  tmp <- tryCatch(
    sofa::db_info(src$con, dbname = key, ...),
    error = function(e) e)
  return(!inherits(tmp, "error"))
}

#' @export
docdb_exists.src_elastic <- function(src, key, ...) {
  return(elastic::index_exists(src$con, key, ...))
}

#' @export
docdb_exists.src_mongo <- function(src, key, ...) {
  # need to check if any document exists
  result <- try(docdb_query(
    src = src, key = key, query = '{}',
    fields = '{"_id": 1}', limit = 1L),
    silent = TRUE, ...)
  return(nrow(result) > 0L)
}

#' @export
docdb_exists.src_sqlite <- function(src, key, ...) {
  return(any(key == DBI::dbListTables(src$con, ...)))
}
