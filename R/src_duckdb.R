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
#' @details Uses [duckdb::duckdb()] as backend. \pkg{nodbi} creates or
#' uses a DuckDB table, with columns `_id` and `json` created and used
#' by package `nodbi`, applying SQL functions as per
#' <https://duckdb.org/docs/extensions/json> to the `json` column.
#' Each row in the table represents a `JSON` document.
#' Any root-level `_id` is extracted from the document(s) and used for
#' column `_id`, otherwise a UUID is created as `_id`.
#' The table is indexed on `_id`.
#' For a benchmark, see <https://github.com/ropensci/nodbi#benchmark>.
#'
#' @return A `nodbi` source object
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

  # test and advise
  xtmsg <- function() {
    try(DBI::dbDisconnect(con, shutdown = TRUE), silent = TRUE)
    stop(
    "DuckDB extension JSON not loadable. To install it, run ",
    "DBI::dbExecute(duckdb::dbConnect(duckdb::duckdb()), 'INSTALL json;') or ",
    "install.packages('duckdb', repos = 'https://duckdb.r-universe.dev')",
    call. = FALSE)
  }
  #
  tmp <- DBI::dbGetQuery(con, 'SELECT * FROM duckdb_extensions();')
  if (nrow(tmp)) tmp <- tmp[tmp[["extension_name"]] == "json", , drop = TRUE]
  if (!tmp$installed) xtmsg()
  if (!tmp$loaded) {
    if (inherits(try(DBI::dbExecute(con, "LOAD json;"),
      silent = TRUE), "try-error"))  xtmsg()
  }

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
  dbver <- try(DBI::dbGetQuery(x$con, "PRAGMA version;")[[
    "library_version"]], silent = TRUE)
  if (inherits(dbver, "try-error")) dbver <- "unknown"

  cat(sprintf(
    "src: duckdb\nDatabase: %s\nSize: %s MB\nVersion: %s",
    dbdir, round(as.integer(size / 1000^6)), dbver
  ))

}

