#' Setup a RSQLite database connection
#'
#' @export
#'
#' @param dbname (character) name of database file,
#'   defaults to ":memory:" for an in-memory database,
#'   see [RSQLite::SQLite()]
#' @param ... additional named parameters passed
#'   on to [RSQLite::SQLite()]
#'
#' @details Uses \pkg{RSQLite} as backend. \pkg{nodbi} creates or uses
#' an SQLite table, with columns `_id` and `json` created and used by
#' package `nodbi`, applying SQL functions as per
#' <https://www.sqlite.org/json1.html> to the `json` column.
#' Each row in the table represents a `JSON` document.
#' Any root-level `_id` is extracted from the document(s) and used for
#' column `_id`, otherwise a UUID is created as `_id`.
#' The table is indexed on `_id`.
#' For a benchmark, see <https://github.com/ropensci/nodbi#benchmark>
#'
#' @return A `nodbi` source object
#'
#' @examples \dontrun{
#' con <- src_sqlite()
#' print(con)
#' }
#'
src_sqlite <- function(dbname = ":memory:", ...) {

  # check minimum version
  pkgNeeded("RSQLite", "2.3.6")

  # open connection
  con <- DBI::dbConnect(
    drv = RSQLite::SQLite(),
    dbname = dbname,
    ...)

  # enable regular expressions
  RSQLite::initRegExp(db = con)

  # enable uuid for csv lines import
  featUuid <- pkgNeeded("RSQLite", "2.3.7.9014", FALSE)
  if (featUuid) {
    RSQLite::initExtension(db = con, extension = "uuid")
    message(
      "RSQLite version has enabled accelerating ",
      "docdb_create() and docdb_update() functions ",
      "when used with value = <NDJSON file name>."
    )
  }

  # set timeout for concurrency to 10s
  DBI::dbExecute(con, "PRAGMA busy_timeout = 10000;")

  # ensure disconnect
  reg.finalizer(
    e = globalenv(),
    f = closeNodbiConnections,
    onexit = TRUE
  )

  # return standard nodbi structure
  structure(list(
    con = con,
    dbname = dbname,
    featUuid = featUuid,
    ...),
    class = c("src_sqlite", "docdb_src"))

}

#' @export
print.src_sqlite <- function(x, ...) {

  dbname <- x$dbname
  dbsize <- file.size(dbname)
  # RSQLite::rsqliteVersion() was introduced with
  # same version that introduced json1 extension
  srv <- rev(RSQLite::rsqliteVersion())[1]
  cat(sprintf(
    "src: sqlite\nSQLite library version: %s\n size: %s MB\n dbname: %s\n",
    srv, signif(dbsize / 10^6, digits = 3L), dbname))

  if (grepl(":memory:", dbname)) {
    warning(
      "Database is only in memory, will not persist after R ends! Consider to copy ",
      "it with \nRSQLite::sqliteCopyDatabase(\n",
      "  from = <your nodbi::src_sqlite() object>$con, \n",
      "  to = <e.g. RSQLite::dbConnect(RSQLite::SQLite(), 'local_file.db')>\n",
      "  )",
      call. = FALSE)
  }

}
