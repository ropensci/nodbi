#' Check if a database exists
#'
#' @export
#' @param src source object, result of call to src
#' @param key (chartacter) A key. ignored for mongo
#' @param ... Ignored for now
#' @template deets
#' @note no docdb_exists method for MongoDB at this time
#' @return logical, `TRUE` or `FALSE`
#' @examples \dontrun{
#' # CouchDB
#' (src <- src_couchdb())
#' docout <- docdb_create(src, key = "mtcars2", value = mtcars)
#' docdb_exists(src, "mtcars2")
#' docdb_exists(src, "asdfadf")
#'
#' # etcd
#' # src <- src_etcd()
#' # docdb_exists(src, "/hello")
#' # docdb_exists(src, "/asdfadsf")
#'
#' # Elasticsearch
#' (src <- src_elastic())
#' if (docdb_exists(src, "iris")) docdb_delete(src, "iris")
#' docdb_exists(src, "iris")
#' docdb_create(src, "iris", iris)
#' docdb_exists(src, "iris")
#' docdb_exists(src, "adfadf")
#'
#' # Redis
#' (src <- src_redis())
#' docdb_create(src, "mtcars", mtcars)
#' docdb_exists(src, "mtcars")
#' docdb_exists(src, "asdfasf")
#' }
docdb_exists <- function(src, key, ...){
  UseMethod("docdb_exists")
}

#' @export
docdb_exists.src_couchdb <- function(src, key, ...) {
  assert(key, 'character')
  tmp <- tryCatch(sofa::db_info(src$con, dbname = key, ...), 
    error = function(e) e)
  !inherits(tmp, "error")
}

#' @export
docdb_exists.src_etcd <- function(src, key, ...) {
  assert(key, 'character')
  tmp <- tryCatch(src$key(key, ...), error = function(e) e)
  !inherits(tmp, "error")
}

#' @export
docdb_exists.src_elastic <- function(src, key, ...) {
  assert(key, 'character')
  elastic::index_exists(src$con, key, ...)
}

#' @export
docdb_exists.src_redis <- function(src, key, ...) {
  assert(key, 'character')
  switch(as.character(src$con$EXISTS(key)), "1" = TRUE, "0" = FALSE)
}

# docdb_exists.src_mongo <- function(src, key, ...) return(TRUE)
