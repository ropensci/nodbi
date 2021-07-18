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
print.src_redis <- function(x, ...) {
  config <- x$con$config()
  keys <- grep("feature:", unlist(x$con$KEYS("*")),
    value = TRUE, invert = TRUE)
  cat(sprintf("src: %s %s [%s:%d]\n",
              x$type, x$version, config$host, config$port))
  cat(doc_wrap("keys: ", paste0(keys, collapse = ", "),
               width = 80), "\n", sep = "")
}
