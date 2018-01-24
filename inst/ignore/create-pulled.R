#' @export
docdb_create.src_riak <- function(src, key, value, ...){
  stopifnot(is.data.frame(value))
  stopifnot(length(attr(src, "dbs")) == 1)
  src$con$create(bucket = attr(src, "dbs"), key = key,
                  body = reeack::riak_serialize(value),
                  content_type = "text/plain", ...)
}
