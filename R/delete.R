#' Delete documents
#'
#' @export
#' @param src source object, result of call to src, an
#' object of class `docdb_src`
#' @param key (chartacter) A key. ignored for mongo
#' @param ... Ignored for now
#' @template deets
#' @examples \dontrun{
#' # couchdb
#' (src <- src_couchdb())
#' docdb_create(src, "mtcars", mtcars)
#' docdb_get(src, "mtcars")
#' docdb_delete(src, "mtcars")
#'
#' # etcd
#' # src <- src_etcd()
#' # docdb_create(src, "/iris", iris)
#' # docdb_get(src, "/iris")
#' # docdb_delete(src, "/iris")
#'
#' # elasticsearch
#' src <- src_elastic()
#' docdb_create(src, "iris", iris)
#' docdb_get(src, "iris")
#' docdb_delete(src, "iris")
#'
#' # Redis
#' src <- src_redis()
#' docdb_create(src, key = "mtcars", value = mtcars)
#' docdb_get(src, "mtcars")
#' docdb_delete(src, "mtcars")
#'
#' # mongo
#' src <- src_mongo("stuff")
#' docdb_create(src, "iris", iris)
#' docdb_get(src, "iris")
#' docdb_delete(src)
#' 
#' # SQLite
#' src <- src_sqlite()
#' docdb_create(src, "iris", iris)
#' docdb_get(src, "iris")
#' docdb_delete(src, "iris")
#' }
docdb_delete <- function(src, key, ...){
  UseMethod("docdb_delete")
}

#' @export
docdb_delete.src_couchdb <- function(src, key, ...) {
	assert(key, 'character')
  sofa::db_delete(src$con, dbname = key, ...)
}

#' @export
docdb_delete.src_etcd <- function(src, key, ...) {
	assert(key, 'character')
  src$delete(key, dir = TRUE, recursive = TRUE, ...)
}

#' @export
docdb_delete.src_elastic <- function(src, key, ...) {
	assert(key, 'character')
  elastic::index_delete(src$con, key, verbose = FALSE)
}

#' @export
docdb_delete.src_redis <- function(src, key, ...) {
	assert(key, 'character')
  src$con$DEL(key)
}

#' @export
docdb_delete.src_mongo <- function(src, key, ...) {
  src$con$drop()
}

#' @export
docdb_delete.src_sqlite <- function(src, key, ...) {
  assert(key, 'character')
  
  # make dotted parameters accessible
  tmpdots <- list(...)
  
  # if valid query, delete document(s), not table
  if (!is.null(tmpdots$query) && 
      jsonlite::validate(tmpdots$query)) {
    
    # get _id's of document to be deleted
    tmpids <- docdb_query(src = src, 
                          key = key, 
                          query = tmpdots$query, 
                          fields = '{"_id": 1}')[["_id"]]
    
    # create delete
    statement <- paste0("DELETE FROM ", key, " WHERE _id IN (", 
                        paste0('"', tmpids, '"', collapse = ','), ");")

    # do delete
    DBI::dbExecute(conn = src$con,
                   statement = statement)
    
  } else {
    
    # remove table
    DBI::dbRemoveTable(conn = src$con, 
                       name = key)
    
  }
}
