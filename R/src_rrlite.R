##' Setup an rrlite database connection
##' @title Setup an rrlite database connection
##' @param filename Name of the database to use
##' @param db Prefix for the keys
##' @export
##' @examples
##' con <- src_rrlite()
##' class(con)
src_rrlite <- function(filename = ":memory:", db = NULL) {
  con <- rrlite::hirlite(filename)
  ret <- list(type="rrlite",
              version=packageVersion("rrlite"),
              con=con,
              db=db)
  class(ret) <- c("src_rrlite", "docdb_src")
  ret
}

##' @export
print.docdb_src_rrlite <- function(x, ...) {
  cat(sprintf("src: %s %s [%s]\n",
              x$type,
              x$version, x$con$context$path))
}
