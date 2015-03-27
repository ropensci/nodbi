#' Setup an etcd database connection
#'
#' @export
#' @examples \dontrun{
#' (src <- src_etcd())
#' class(src)
#' }
src_etcd <- function() {
  ver <- etseed::version()
  structure(list(version=ver, type="etcd"), class=c("src_etcd","docdb_src"))
}

##' @export
print.src_etcd <- function(x, ...) {
  cat(sprintf("src: %s\n", x$version))
}
