#' Setup a DuckDB database connection
#'
#' @export
#'
#' @inheritParams duckdb::duckdb
#'
#' @inheritParams DBI::dbConnect
#'
#' @param ... Additional named parameters passed on to [DBI::dbConnect()]
#'
#' @details Uses [duckdb::duckdb()] under the hood
#'
#' @examples \dontrun{
#' con <- src_duckdb()
#' print(con)
#' }
src_duckdb <- function(
    drv = duckdb::duckdb(),
    dbdir = attr(drv, "dbdir"),
    ...) {

  con <- duckdb::dbConnect(
    drv = drv,
    dbdir = dbdir,
    ...
    )

  # ensure disconnect
  reg.finalizer(
    e = globalenv(),
    f = closeNodbiConnections,
    onexit = TRUE
  )

  # potential security concern with
  # storing the full connection string
  structure(list(con = con,
                 dbdir = dbdir,
                 ...),
            class = c("src_duckdb", "docdb_src"))

}

#' @export
print.src_duckdb <- function(x, ...) {

  dbdir <- attr(attr(x$con, "driver"), "dbdir")
  size <- switch(dbdir,
    ":memory:" = utils::object.size(x),
    file.size(dbdir))

  cat(sprintf(
    "src: duckdb\nDatabase: %s\nSize: %s MB\n",
    dbdir, round(as.integer(size / 1000^6))
  ))

}

