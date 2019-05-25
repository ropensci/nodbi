#' Create documents
#'
#' @export
#' @param src source object, result of call to an [src] function
#' @param key (chartacter) A key. ignored for mongo
#' @param value (data.frame) A single data.frame
#' @param ... Ignored
#' @template deets
#' @examples \dontrun{
#' # CouchDB
#' src <- src_couchdb()
#' docdb_create(src, key="mtcars2", value=mtcars)
#' docdb_get(src, "mtcars2")
#'
#' # etcd
#' # src <- src_etcd()
#' # docdb_create(src, key = "/newmtcars7", value = mtcars)
#' # docdb_get(src, "/newmtcars7")
#'
#' # Elasticsearch
#' src <- src_elastic()
#' docdb_create(src, key = "mtcars", value = mtcars)
#' docdb_create(src, key = "iris", value = iris)
#' docdb_create(src, key = "diamonds_small", value = diamonds[1:3000L,])
#'
#' # Redis
#' src <- src_redis()
#' docdb_create(src, key = "mtcars", value = mtcars)
#' docdb_get(src, "mtcars")
#' docdb_delete(src, "mtcars")
#'
#' # MongoDB
#' src <- src_mongo()
#' docdb_create(src, key = "mtcars", value = mtcars)
#' docdb_get(src, "mtcars")
#' 
#' # SQLite
#' src <- src_sqlite()
#' docdb_create(src, key = "mtcars", value = mtcars)
#' docdb_create(src, key = "mtcars", value = contacts)
#' docdb_get(src, "mtcars")
#' }
docdb_create <- function(src, key, value, ...){
  UseMethod("docdb_create")
}

#' @export
docdb_create.src_couchdb <- function(src, key, value, ...) {
  assert(value, 'data.frame')
  trycr <- tryCatch(sofa::db_create(src$con, dbname = key),
    error = function(e) e)
  invisible(sofa::db_bulk_create(src$con, dbname = key, doc = value, ...))
}

#' @export
docdb_create.src_etcd <- function(src, key, value, ...){
  assert(value, 'data.frame')
  invisible(src$create(key = key, dir = TRUE, ...))
  for (i in seq_len(NROW(value))) {
    src$create_inorder(key, jsonlite::toJSON(value[i, ]), ...)
  }
}

#' @export
docdb_create.src_elastic <- function(src, key, value, ...){
  assert(value, 'data.frame')
  elastic::index_create(src$con, index = key, verbose = FALSE)
  invisible(elastic::docs_bulk(src$con, value, index = key))
}

#' @export
docdb_create.src_redis <- function(src, key, value, ...) {
  assert(value, 'data.frame')
  src$con$SET(key, redux::object_to_string(value), ...)
}

#' @export
docdb_create.src_mongo <- function(src, key, value, ...){
  assert(value, 'data.frame')
  src$con$insert(value, ...)
}

#' @export
docdb_create.src_sqlite <- function(src, key, value, ...){
  
  assert(value,  "data.frame")
  assert(key, "character")
  
  ### if table does not exist, create one
  if (!docdb_exists(src, key)) {
    
    # defining the standard for a nodbi json table in sqlite:
    # CREATE TABLE mtcars ( _id TEXT PRIMARY_KEY NOT NULL, json JSON );
    # CREATE UNIQUE INDEX mtcars_index ON mtcars ( _id );
    
    DBI::dbExecute(
      conn = src$con, 
      statement = paste0("CREATE TABLE ", key, 
                         " ( _id TEXT PRIMARY_KEY NOT NULL,", 
                         "  json JSON);"))
    
    DBI::dbExecute(
      conn = src$con, 
      statement = paste0("CREATE UNIQUE INDEX ", 
                         key, "_index ON ",
                         key, " ( _id );"))
  }
  
  ### return if no value provided, 
  # such as to create empty table
  if (is.null(value)) return(invisible(0L))
  
  # add _id column if not yet in data.frame 'value'
  # and fill column _id with random identifiers
  idcol <- grep("_id", names(value))
  if (!length(idcol)) {
    
    value <- data.frame(
      "_id" = sapply(seq_len(nrow(value)), 
                     function(x) rand_id()),
      value, 
      stringsAsFactors = FALSE,
      check.names = FALSE)
    
    idcol <- 1
  }
  
  # add one document(s) for each row in data.frame 'value'
  nrowaffected <- sapply(seq_len(nrow(value)), function(i) {
    
    if ("try-error" %in% 
        class(try(jsonlite::validate(value[i, -idcol]), 
                  silent = TRUE))) {
      
      # for regular data frame
      DBI::dbExecute(
        conn = src$con, 
        statement = paste0("INSERT INTO ", key, " (_id, json) ", 
                           "values ('", value[i,  idcol], "', '", 
                           proc_doc(value[i, -idcol]), "');"
        ))
      
    } else {
      
      # when dataframe is already json
      
      # minify
      value[i, -idcol] <- jsonlite::minify(value[i, -idcol])
      
      # check if _id's in json and get them
      subids <- gregexpr('"_id":".*?"', value[i, -idcol])
      subids <- regmatches(value[i, -idcol], subids)
      subids <- sub(".*:\"(.*)\".*", "\\1", unlist(subids))
      
      if (length(subids)) {
        
        # if not in square brackets, add them 
        if (!grepl("^\\[.*\\]$", value[i, -idcol]))
          value[i, -idcol] <- paste0('[', value[i, -idcol], ']')
        
        # splice value element into json elements
        subvalue <- jsonlite::fromJSON(value[i, -idcol], simplifyVector = FALSE)
        subvalue <- sapply(subvalue, function(x) jsonlite::toJSON(x, auto_unbox = TRUE))
        
        sapply(seq_along(subvalue), function(ii){
          
          statement <- paste0("INSERT INTO ", key, " (_id, json) ", 
                              "values ('", subids[ii], "', '", 
                              subvalue[ii], "');")
          
          # TODO remove
          if (getOption("verbose")) message(statement)
          
          DBI::dbExecute(
            conn = src$con, 
            statement = statement)
          
        })
        
      } else {
        
        statement = paste0("INSERT INTO ", key, " (_id, json) ", 
                           "values ('", value[i,  idcol], "', '", 
                           value[i, -idcol], "');")
        
        # TODO remove
        if (getOption("verbose")) message(statement)

        DBI::dbExecute(
          conn = src$con, 
          statement = statement)
        
      }
    }
  }
  )
  
  # return number of created rows in table
  return(invisible(sum(nrowaffected, na.rm = TRUE)))
  
}

## helpers --------------------------------------

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

rand_id <- function() {
  v = c(sample(0:9,     12, replace = TRUE),
        sample(letters, 12, replace = TRUE))
  paste0(sample(v), collapse = "")
}
