#' List containers in database
#'
#' @inheritParams docdb_create
#'
#' @param ... Passed to function [DBI::dbListTables()]
#'
#' @return Vector of names of containers that can be
#' used as parameter `key` with other functions such as
#' [docdb_create()].
#'
#' @export
#'
#' @examples \dontrun{
#' src <- src_sqlite()
#' docdb_create(src, "iris", iris)
#' docdb_list(src)
#' }
docdb_list <- function(src, ...) {
  assert(src, "docdb_src")
  UseMethod("docdb_list", src)
}

#' @export
docdb_list.src_couchdb <- function(src, ...) {

  result <- sofa::db_list(src$con)
  result <- result[result != "_users"]
  return(result)

}

#' @export
docdb_list.src_elastic <- function(src, ...) {

  result <- elastic::aliases_get(src$con)
  result <- names(result)
  return(result)

}

#' @export
docdb_list.src_mongo <- function(src, ...) {

  result <- src$con$run(
    command = '{"listCollections":1}',
    simplify = TRUE)

  result <- unlist(result, use.names = TRUE)
  result <- result[grepl("atch[.]name[0-9]*$", names(result))]
  result <- result[result != "system.indexes"]
  result <- unname(result)

  return(result)
}

#' @export
docdb_list.src_sqlite <- function(src, ...) {
  return(DBI::dbListTables(src$con, ...))
}

#' @export
docdb_list.src_postgres <- function(src, ...) {
  return(DBI::dbListTables(src$con, ...))
}

#' @export
docdb_list.src_duckdb <- function(src, ...) {
  return(DBI::dbListTables(src$con, ...))
}
