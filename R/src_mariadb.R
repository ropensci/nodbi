#' Setup a RMariaDB database connection
#'
#' @export
#'
#' @param dbname (character) name of database file,
#'   defaults to ":memory:" for an in-memory database,
#'   see [RMariaDB::MariaDB()]
#' @param ... additional named parameters passed
#'   on to [RMariaDB::MariaDB()]
#'
#' @details Uses \pkg{RSMariaDB} as backend; minimum MariaDB 12.3 required.
#' \pkg{nodbi} creates or uses
#' a table with columns `_id` and `json`, created and used by
#' package `nodbi`, applying SQL or JSON functions as per
#' https://mariadb.com/docs/server/reference/sql-functions/special-functions/json-functions
#' to the `json` column.
#' Each row in the table represents a `JSON` document.
#' Any root-level `_id` is extracted from the document(s) and used for
#' column `_id`, otherwise a UUID is created as `_id`.
#' The table is indexed on `_id`.
#' For a benchmark, see <https://github.com/ropensci/nodbi#benchmark>.
#'
#' @return A `nodbi` source object
#'
#' @examples \dontrun{
#' con <- src_sqlite()
#' print(con)
#' }
#'
src_mariadb <- function(dbname = "test", ...) {

  # check minimum version
  pkgNeeded("RMariaDB", "1.3.0")

  # open connection
  con <- DBI::dbConnect(
    drv = RMariaDB::MariaDB(),
    dbname = dbname,
    ...)

  # https://mariadb.com/docs/server/reference/sql-functions/special-functions/json-functions
  # https://mariadb.com/docs/server/reference/sql-functions/special-functions/json-functions/jsonpath-expressions

  # ensure disconnect
  reg.finalizer(
    e = globalenv(),
    f = closeNodbiConnections,
    onexit = TRUE
  )

  # get info
  info <- DBI::dbGetQuery(
    conn = con,
    statement = "SHOW VARIABLES;")

  # MySQLdb or MariaDB
  dbver <- info$Value[grepl("^innodb_version$|^version$", info$Variable_name)]
  dbver <- gsub("(.*?)-.*", "\\1", dbver)
  dbver <- gsub("[^.0-9]", "", dbver)
  dbver <- unique(dbver)[1]

  # check versions
  if (!package_version(dbver) > '12.3') stop(
    "Minimum version is 12.3, server reports ", dbver
  )

  # return standard nodbi structure
  structure(
    list(
      con = con,
      dbname = dbname,
      dbver = dbver,
      ...),
    class = c("src_mariadb", "docdb_src"))

}

#' @export
print.src_mariadb <- function(x, ...) {

  dbsize <- NA_real_

  info <- try(
    suppressWarnings(
      DBI::dbGetQuery(
        conn = x$con,
        statement = paste0(
          'SELECT table_schema AS "DB", ',
          'SUM(data_length + index_length) \'B\' ',
          'FROM information_schema.tables;'
        )
      )
    )
  )

  if (!inherits(info, "try-error")) dbsize <- info$B

  srcInfo("MariaDB", x$dbver, x$dbname, dbsize)

}
