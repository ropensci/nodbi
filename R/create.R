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
#' docdb_create(src, key = "mtcars", value = data.frame(contacts, stringsAsFactors = FALSE))
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
    
    ### defining the standard for a nodbi json table in sqlite:
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
  
  ### convert dataframe rows into json
  # respect any pre-existing json
  if (all(sapply(value, is.character)) &&
      all(sapply(value, jsonlite::validate))) {
    
    # process json row by row
    value2 <- sapply(X = seq_len(nrow(value)), 
                     FUN = function(x) {
                       
                       # get row from data frame
                       tmp <- value[x, ]
                       
                       # minify for regexp
                       tmp <- jsonlite::minify(tmp)
                       
                       # check if _id's in json and get them
                       subids <- gregexpr('"_id":".*?"', tmp)
                       subids <- regmatches(tmp, subids)
                       subids <- sub(".*:\"(.*)\".*", "\\1", unlist(subids))
                       #
                       if (length(subids)) {
                         
                         # if not in square brackets, add them 
                         if (!grepl("^\\[.*\\]$", tmp)) {
                           tmp <- paste0('[', tmp, ']')
                         }
                         
                         # remove _ids
                         tmp <- gsub('"_id":".*?",', "", tmp)
                         
                         # splice tmp element into json elements and merge again
                         subvalue <- jsonlite::fromJSON(tmp, simplifyVector = FALSE)
                         subvalue <- sapply(subvalue, function(x) jsonlite::toJSON(x, auto_unbox = TRUE))
                         
                         # output
                         data.frame("_id" = subids, 
                                    "json" = subvalue, 
                                    stringsAsFactors = FALSE,
                                    check.names = FALSE)
                         
                       } else {
                         
                         # use json as-is
                         tmp
                       }
                     })
    
    # value included json subelements
    value <- data.frame(
      "_id" = value2[[1]],  
      "json" = value2[[2]],
      stringsAsFactors = FALSE,
      check.names = FALSE)
    
  } else { 
    
    # no json in dataframe, 
    # transform row into json
    dump <- tempfile()
    dumpcon <- file(dump)
    
    # this is fasted using ndjson
    jsonlite::stream_out(
      x = value, 
      con = dumpcon, 
      verbose = FALSE)

    value <- data.frame(
      "_id" = ifelse(
        test = rep(any(grepl("_id", names(value))), nrow(value)),
        yes = value[["_id"]], 
        no = row.names(value)),
      "json" = readLines(dump),
      stringsAsFactors = FALSE,
      check.names = FALSE)
    
    # cleanup
    unlink(dump)
  }
  
  ### load into database
  nrowaffected <- 
    DBI::dbAppendTable(
      conn = src$con, 
      name = key, 
      value = value,
      ...)
  
  return(invisible(nrowaffected))
  
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
