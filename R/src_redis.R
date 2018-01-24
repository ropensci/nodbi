#' Setup an Redis database connection
#' @title Setup an Redis database connection
#' @param ... Redis connection configuration options. See [redux::redis_config] 
#' for more information
#' @export
#' @examples
#' (con <- src_redis())
#' class(con)
src_redis <- function(...) {
  con <- redux::hiredis(...)
  ret <- list(type = "redis",
              version = utils::packageVersion("redux"),
              con = con)
  class(ret) <- c("src_redis", "docdb_src")
  ret
}

#' @export
print.docdb_src_redis <- function(x, ...) {
  cat(sprintf("src: %s %s [%s:%d]\n",
              x$type, x$version, x$con$host, x$con$port))
}

# Setup an rlite database connection
# @title Setup an rlite database connection
# @param ... named config options passed on to
# \code{\link[redux]{redis_config}}
# @export
# @examples
# con <- src_rlite()
# class(con)
# src_rlite <- function(...) {
#   con <- rrlite::hirlite(...)
#   ret <- list(type = "redis",
#               version = utils::packageVersion("rrlite"),
#               con = con)
#   class(ret) <- c("src_rlite", "src_redis", "docdb_src")
#   ret
# }

# print.docdb_src_rlite <- function(x, ...) {
#   cat(sprintf("src: %s %s [%s]\n",
#               x$type,
#               x$version, x$con$context$path))
# }


# using redux
r <- redux::hiredis()
r$PING()
r$SET("foo", "bar")
r$GET("foo")
