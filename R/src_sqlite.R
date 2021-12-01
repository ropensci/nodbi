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
#' @details uses \pkg{RSQLite} under the hood
#'
#' @examples \dontrun{
#' con <- src_sqlite()
#' print(con)
#' }
#'
src_sqlite <- function(dbname = ":memory:",
                       ...) {

  # open connection
  con <- DBI::dbConnect(
    drv = RSQLite::SQLite(),
    dbname = dbname,
    ...)

  # check if json1 extension is supported
  if (inherits(
    try(
      DBI::dbGetQuery(
        conn = con,
        statement = paste0("SELECT json_patch('{\"a\":1}','{\"a\":9}');")
      ), silent = TRUE
  ), "try-error")) {
    stop("SQLite does not have json1 extension enabled. Call ",
         "install.packages('RSQLite') to install a current version.")
  }

  # check if regular expressions are supported (RSQLite >= 2.1.2)
  if (inherits(
    try({
      RSQLite::initRegExp(db = con)
      DBI::dbExecute(
        conn = con,
        statement = paste0('SELECT * FROM (VALUES ("Astring")) WHERE 1 REGEXP "[A-Z]+";'))
    }, silent = TRUE
  ), "try-error")) {
    stop("SQLite does not support REGEXP. Call ",
         "install.packages('RSQLite') to install a current version.")
  }

  # set timeout for concurrency to 10s
  DBI::dbExecute(con, "PRAGMA busy_timeout = 10000;")

  # return standard nodbi structure
  structure(list(con = con,
                 dbname = dbname,
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
    "src: sqlite\nSQLite library version: %s\n size: %s kBytes\n dbname: %s\n",
    srv, dbsize / 2^10, dbname))

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
