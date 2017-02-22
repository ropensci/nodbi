#' Create documents
#'
#' @export
#' @param src source object, result of call to src
#' @param key A key. See Details.
#' @param value A value
#' @param ... Ignored
#' @details Note that with etcd, you have to prefix a key with a forward slash.
#' @examples \dontrun{
#' # CouchDB
#' src <- src_couchdb()
#' docdb_create(src, key="mtcars2", value=mtcars)
#' docdb_get(src, "mtcars2")
#'
#' # Etcd
#' src <- src_etcd()
#' docdb_create(src, key = "/newmtcars7", value = mtcars)
#' docdb_get(src, "/newmtcars7")
#'
#' # Elasticsearch
#' src <- src_elasticsearch()
#' docdb_create(src, key = "mtcars", value = mtcars)
#' docdb_create(src, key = "iris", value = iris)
#' docdb_create(src, key = "diamonds_small", value = diamonds[1:3000L,])
#'
#' # Redis
#' ### server
#' src1 <- src_redis()
#' docdb_create(src1, key = "mtcars", value = mtcars)
#' docdb_get(src1, "mtcars")
#' docdb_delete(src1, "mtcars")
#'
#' ### serverless
#' src2 <- src_rlite()
#' docdb_create(src2, key = "mtcars", value = mtcars)
#' docdb_get(src2, "mtcars")
#' docdb_delete(src2, "mtcars")
#'
#' # MongoDB
#' src <- src_mongo()
#' docdb_create(src, key = "mtcars", value = mtcars)
#' docdb_get(src, "mtcars")
#' }
docdb_create <- function(src, key, value, ...){
  UseMethod("docdb_create")
}

#' @export
docdb_create.src_couchdb <- function(src, key, value, ...) {
  if (!key %in% attr(src, "dbs")) sofa::db_create(src[[1]], dbname = key)
  invisible(sofa::db_bulk_create(src[[1]], dbname = key, doc = value, ...))
}

#' @export
docdb_create.src_etcd <- function(src, key, value, ...){
  invisible(src$create(key = key, dir = TRUE, ...))
  cl <- class(value)
  switch(cl,
         data.frame = {
           for (i in seq_along(value)) {
             src$create_inorder(key, jsonlite::toJSON(value[i, ]), ...)
             # etseed::create(paste0(key, "/", i), jsonlite::toJSON(value[i, ]))
           }
         }
  )
}

#' @export
docdb_create.src_elasticsearch <- function(src, key, value, ...){
  elastic::index_create(index = key, verbose = FALSE)
  switch(
    class(value),
    data.frame = {
      invisible(elastic::docs_bulk(value, index = key))
    },
    stop("only objects of class 'data.frame' supported")
  )
}

#' @export
docdb_create.src_redis <- function(src, key, value, ...) {
  src$con$SET(key, RedisAPI::object_to_string(value), ...)
}

#' @export
docdb_create.src_mongo <- function(src, key, value, ...){
  stopifnot(is.data.frame(value))
  src$con$insert(value, ...)
}

# make_bulk("mtcars", mtcars, "~/mtcars.json")
# make_bulk("iris", iris, "~/iris.json")
# make_bulk("diamonds", diamonds, "~/diamonds.json")
make_bulk <- function(key, value, filename = "~/docdbi_bulk.json") {
  unlink(filename)
  for (i in 1:NROW(value)) {
    dat <- list(index = list(`_index` = key, `_type` = key, `_id` = i - 1))
    cat(proc_doc(dat), sep = "\n", file = filename, append = TRUE)
    cat(proc_doc(value[i,]), sep = "\n", file = filename, append = TRUE)
  }
}

proc_doc <- function(x){
  b <- jsonlite::toJSON(x, auto_unbox = TRUE)
  gsub("\\[|\\]", "", as.character(b))
}
