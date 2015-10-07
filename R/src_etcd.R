#' Setup an Etcd database connection
#'
#' @export
#' @examples \dontrun{
#' (src <- src_etcd())
#' class(src)
#' }
src_etcd <- function() {
  ver <- etseed::version()
  structure(c(ver, type = "etcd"), class = c("src_etcd", "docdb_src"))
}

#' @export
print.src_etcd <- function(x, ...) {
  cat("src:\n")
  cat(sprintf("  etcd server: %s\n", x$etcdserver))
  cat(sprintf("  etcd cluster: %s\n", x$etcdcluster))
}
