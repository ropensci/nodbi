##' Setup an Redis database connection
##' @title Setup an Redis database connection
##' @param host Host URI
##' @param port Port number
##' @export
##' @examples
##' (con <- src_redis())
##' class(con)
src_redis <- function(host="127.0.0.1", port=6379) {
  con <- RedisAPI::rcppredis_hiredis(host = host, port = port)
  ret <- list(type = "redis",
              version = utils::packageVersion("RedisAPI"),
              con = con)
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
##' @param ... named config options passed on to
##' \code{\link[redux]{redis_config}}
##' @export
##' @examples
##' con <- src_rlite()
##' class(con)
src_rlite <- function(...) {
  con <- rrlite::hirlite(...)
  ret <- list(type = "redis",
              version = utils::packageVersion("rrlite"),
              con = con)
  class(ret) <- c("src_rlite", "src_redis", "docdb_src")
  ret
}

##' @export
print.docdb_src_rlite <- function(x, ...) {
  cat(sprintf("src: %s %s [%s]\n",
              x$type,
              x$version, x$con$context$path))
}
