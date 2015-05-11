##' Setup an Redis database connection
##' @title Setup an Redis database connection
##' @param filename Name of the database to use
##' @param db Prefix for the keys
##' @export
##' @examples
##' con <- src_redis()
##' class(con)
src_redis <- function(host="127.0.0.1", port=6379) {
  con <- RedisAPI::hiredis(host, port)
  ret <- list(type="redis",
              version=packageVersion("RedisAPI"),
              con=con)
  class(ret) <- c("src_redis", "docdb_src")
  ret
}

##' @export
print.docdb_src_redis <- function(x, ...) {
  cat(sprintf("src: %s %s [%s:%d]\n",
              x$type, x$version, x$con$host, x$con$port))
}

##' Setup an rlite database connection
##' @title Setup an rlite database connection
##' @param filename Name of the database to use
##' @export
##' @examples
##' con <- src_rlite()
##' class(con)
src_rlite <- function(filename = ":memory:") {
  con <- rrlite::hirlite(filename)
  ret <- list(type="redis",
              version=packageVersion("rrlite"),
              con=con)
  class(ret) <- c("src_rlite", "src_redis", "docdb_src")
  ret
}

##' @export
print.docdb_src_rlite <- function(x, ...) {
  cat(sprintf("src: %s %s [%s]\n",
              x$type,
              x$version, x$con$context$path))
}
