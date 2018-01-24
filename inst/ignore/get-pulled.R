#' @export
docdb_get.src_redis <- function(src, docid, ...) {
  res <- src$con$GET(docid)
  if (is.null(res)) stop("no matching result found")
  RedisAPI::string_to_object(res)
}

#' @export
docdb_get.src_riak <- function(src, docid, ...) {
  reeack::riak_unserialize(src$con$fetch(key = docid))
}
