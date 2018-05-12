#' Setup an Redis database connection
#'
#' @title Setup an Redis database connection
#' @export
#' @param ... Redis connection configuration options. See [redux::redis_config]
#' for more information
#' @details uses \pkg{redux} under the hood; uses [redux::hiredis()] for
#' connecting
#' @examples \dontrun{
#' (con <- src_redis())
#' class(con)
#' }
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
