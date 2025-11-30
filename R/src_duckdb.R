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

  # check minimum version
  pkgNeeded("duckdb", "1.4.0")

  # create connection
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
    "DuckDB extension JSON not loadable. To install it, run \n",
    "DBI::dbExecute(duckdb::dbConnect(duckdb::duckdb()), 'INSTALL json;')",
    call. = FALSE)
  }
  #
  tmp <- DBI::dbGetQuery(con, 'SELECT * FROM duckdb_extensions();')
  if (nrow(tmp)) tmp <- tmp[tmp[["extension_name"]] == "json", , drop = TRUE]
  if (!tmp$installed) xtmsg()
  if (!tmp$loaded) {
    if (inherits(try(DBI::dbExecute(con, "LOAD json;"),
      silent = TRUE), "try-error")) xtmsg()
  }

  # version
  dbver <- try(DBI::dbGetQuery(con, "PRAGMA version;")[[
    "library_version"]], silent = TRUE)
  if (inherits(dbver, "try-error")) dbver <- "0.0"
  # remove non-numbers
  dbver <- gsub("[^-0-9.]", "", dbver)

  # user info
  if (grepl(":memory:", dbdir)) {
    warning(
      "Database is only in memory, will not persist after R ends! Consider copying ",
      "it with \nDBI::dbExecute(\n",
      " conn = <your nodbi::src_duckdb() object>$con,\n",
      " statement = \n  \"ATTACH 'local_file.duckdb';\n",
      "   COPY FROM DATABASE memory TO local_file;\n",
      "   DETACH local_file;\")",
      call. = FALSE)
  }

  # create structure
  structure(
    list(
      con = con,
      dbdir = dbdir,
      dbver = dbver,
      ...),
    class = c("src_duckdb", "docdb_src"))

}

#' @export
print.src_duckdb <- function(x, ...) {

  dbdir <- attr(attr(x$con, "driver"), "dbdir")
  dbsize <- switch(
    dbdir,
    ":memory:" = utils::object.size(x),
    file.size(dbdir))

  srcInfo("DuckDB", x$dbver, dbdir, dbsize)

}

