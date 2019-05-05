#' Setup a sqlite database connection
#'
#' @export
#' 
#' @param dbname (character) name of database file
#' @param ... additional named params passed on to [RSQLite::SQLite()]
#' 
#' @details uses \pkg{RSQLite} under the hood; uses [RSQLite::SQLite()] for
#' connecting
#' 
#' @examples \dontrun{
#' (con <- src_sqlite())
#' print(con)
#' }
src_sqlite <- function(dbname = "test.sqlite", 
                       ...) {

  # full path to dbname
  dbname <- normalizePath(dbname)
  
  # parameter collection is for the 
  # sqlite table to be used, and this
  # is recorded but not used here. 
  
  con <- DBI::dbConnect(drv = RSQLite::SQLite(), 
                        dbname = normalizePath(dbname), 
                        ...)
  
  structure(list(con = con, 
                 dbname = dbname), 
            class = c("src_sqlite", "docdb_src"))

}

#' @export
print.src_sqlite <- function(x, ...) {
  con <- x$con
  dbname <- x$dbname
  dbsize <- file.size(dbname)
  srv <- rev(RSQLite::rsqliteVersion())[1]
  cat(sprintf("SQLite library version: %s, \nsize: %s kBytes, \ndatabase file: %s\n",
              srv, dbsize / 2^10, dbname))
}