#' @export
docdb_delete.src_redis <- function(src, key, ...) {
  src$con$DEL(key)
}

#' @export
docdb_delete.src_riak <- function(src, key, ...) {
  src$con$delete(key, ...)
}
