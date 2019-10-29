#' Delete documents
#'
#' @export
#' @param src source object, result of call to src, an
#' object of class `docdb_src`
#' @param key (character) A key (collection for mongo)
#' @param ... Ignored for now
#' @template deets
#' @examples \dontrun{
#' # couchdb
#' (src <- src_couchdb())
#' docdb_create(src, "mtcars", mtcars)
#' docdb_get(src, "mtcars")
#' docdb_delete(src, "mtcars")
#'
#' # elasticsearch
#' src <- src_elastic()
#' if (docdb_exists(src, "iris")) docdb_delete(src, "iris")
#' docdb_create(src, "iris", iris)
#' Sys.sleep(2)
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
#' src <- src_mongo(collection = "iris")
#' docdb_create(src, "iris", iris)
#' docdb_get(src, "iris")
#' docdb_delete(src, "iris")
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
  
  # check expectations
  if (exists("key", inherits = FALSE) && 
      src$collection != key) 
    message("Parameter 'key' is different from parameter 'collection', ",
            "was given as ", src$collection, " in src_mongo().")
  
  # https://docs.mongodb.com/manual/tutorial/remove-documents/
  # https://jeroen.github.io/mongolite/manipulate-data.html#remove
  
  # make dotted parameters accessible
  tmpdots <- list(...)
  
  # if valid json, try to delete 
  # document(s) instead of collection
  if (!is.null(tmpdots$query) && 
      jsonlite::validate(tmpdots$query)) {
    
    # delete document
    src$con$remove(query = tmpdots$query, 
                   just_one = FALSE)
    
  } else {
    
    # delete collection
    src$con$drop()

  }
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
