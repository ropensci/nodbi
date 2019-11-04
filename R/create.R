#' Create documents
#'
#' @export
#' @param src source object, result of call to an [src] function
#' @param key (character) A key (collection for mongo)
#' @param value (data.frame) A single data.frame
#' @param ... Ignored
#' @template deets
#' @examples \dontrun{
#' # CouchDB
#' src <- src_couchdb()
#' docdb_create(src, key="mtcars2", value=mtcars)
#' docdb_get(src, "mtcars2")
#'
#' # Elasticsearch
#' src <- src_elastic()
#' if (docdb_exists(src, "mtcars")) docdb_delete(src, "mtcars")
#' if (docdb_exists(src, "iris")) docdb_delete(src, "iris")
#' if (docdb_exists(src, "diamonds_small")) docdb_delete(src, "diamonds_small")
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
#' src <- src_mongo(collection = "mtcars")
#' docdb_create(src, key = "mtcars", value = mtcars)
#' docdb_get(src, "mtcars")
#' 
#' # SQLite
#' src <- src_sqlite()
#' if (docdb_exists(src, "mtcars")) docdb_delete(src, "mtcars")
#' docdb_create(src, key = "mtcars", value = mtcars)
#' docdb_get(src, "mtcars")
#' if (docdb_exists(src, "contacts")) docdb_delete(src, "contacts")
#' ## contacts is a dataset included in this package
#' contacts_df <- data.frame(contacts, stringsAsFactors = FALSE)
#' docdb_create(src, key = "contacts", value = contacts_df)
#' docdb_get(src, "contacts")
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
  
  # check expectations
  if (exists("key", inherits = FALSE) && 
      src$collection != key) 
    message("Parameter 'key' is different from parameter 'collection', ",
            "was given as ", src$collection, " in src_mongo().")
  
  # Document identifier is created by mongolite
  # from _id column or row.names of dataframe "value". 
  # If dataframe has _id column, ensure it is character
  # to emulate how row.names is used by mongolite.
  
  if (any(grepl("_id", names(value)))) {
    value[["_id"]] <- as.character(value[["_id"]])
  }
  
  # mongolite: 
  # insert(data, pagesize = 1000, stop_on_error = TRUE, ...)
  # 
  # Insert rows into the collection. 
  # Argument 'data' must be a 
  # - data-frame, 
  # - named list (for single record) or 
  # - character vector with json strings (one string for each row). 
  # 
  # For lists and data frames, arguments 
  # in ... get passed to jsonlite::toJSON
  
  # check if _id in data.frame
  idcol <- grep("_id", names(value))
  valcol <- 1L + ifelse(length(idcol), 1L, 0L)
  
  # Check if data.frame has one or two 
  # columns where the non-_id column is
  # already filled with json strings: 
  if (ncol(value) == (1L + ifelse(length(idcol) != 0L, 1L, 0L)) &&
      all(sapply(value[, valcol], is.character)) &&
      all(sapply(value[, valcol], jsonlite::validate))) {
    
    # True, thus now add json strings as documents.
    # Iterate over rows if any in data.frame value
    nrowaffected <- sapply(seq_len(nrow(value)), function(i) {
      
      # minify
      value[i, valcol] <- as.character(jsonlite::minify(value[i, valcol]))
      
      # check if subids (_id's) in json strings, extract them
      subids <- gregexpr('"_id":".*?"', value[i, valcol])
      subids <- regmatches(value[i, valcol], subids)
      subids <- sub(".*:\"(.*)\".*", "\\1", unlist(subids))
      
      if (length(subids)) {
        
        # if not in square brackets, add them 
        if (!grepl("^\\[.*\\]$", value[i, valcol]))
          value[i, -idcol] <- paste0('[', value[i, valcol], ']')
        
        # splice value element into json elements
        subvalue <- jsonlite::fromJSON(value[i, valcol], simplifyVector = FALSE)
        subvalue <- sapply(subvalue, function(x) jsonlite::toJSON(x, auto_unbox = TRUE))
        
        # iterate over elements and each has an _id
        sapply(seq_along(subvalue), function(ii){
          
          # insert
          src$con$insert(subvalue[ii], ...)$nInserted
          
        })
        
      } else {# no subids
        
        # add _id into beginning of json string, 
        # if the json string does not yet have it.
        tmpvalue <- value[i, valcol, drop = TRUE]
        if (length(idcol) && !grepl('"_id"', tmpvalue)) 
          tmpvalue <- jsonlite::toJSON(c(list("_id" = value[i, idcol, drop = TRUE]), 
                                         jsonlite::fromJSON(tmpvalue)), auto_unbox = TRUE)
        
        # insert
        src$con$insert(data = tmpvalue, ...)$nInserted
        
      }
      
    })
    
  } else {# no character vecto with json strings in data.frame
    
    # standard method to add data.frame
    nrowaffected <- src$con$insert(data = value, ...)$nInserted
    
  }
  
  # return number of created rows in table
  return(invisible(sum(nrowaffected, na.rm = TRUE)))
  
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
  
  # check if _id in data.frame
  idcol <- grep("_id", names(value))
  valcol <- 1L + ifelse(length(idcol), 1L, 0L)
  
  # Check if data.frame has one or two 
  # columns where the non-_id column is
  # already filled with json strings: 
  if (ncol(value) == (1L + ifelse(length(idcol) != 0L, 1L, 0L)) &&
      all(sapply(value[, valcol], is.character)) &&
      all(sapply(value[, valcol], jsonlite::validate))) {
    
    # # convert dataframe rows into json
    # # respect any pre-existing json
    # if (all(sapply(value, is.character)) &&
    #     all(sapply(value, jsonlite::validate))) {
    
    # process json row by row
    value2 <- lapply(X = seq_len(nrow(value)), 
                     FUN = function(x) {
                       
                       # get row from data frame
                       tmp <- value[x, valcol]
                       
                       # minify for regexp
                       tmp <- as.character(jsonlite::minify(tmp))
                       
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
                         
                         # output
                         data.frame("_id" = value[x, idcol], 
                                    "json" = tmp, 
                                    stringsAsFactors = FALSE,
                                    check.names = FALSE)
                         
                       }
                     })
    
    # value included json subelements
    value <- do.call(rbind, value2)
        
  } else { 
    
    # no json in dataframe, 
    # transform row into json
    dump <- tempfile()
    dumpcon <- file(dump)
    
    # this is fastest using ndjson
    if (length(idcol) == 0L) {
      # no idcol
      jsonlite::stream_out(
        x = value,
        con = dumpcon, 
        verbose = FALSE)
    } else {
      # has idcol
      jsonlite::stream_out(
        x = value[ -idcol ], 
        con = dumpcon, 
        verbose = FALSE)
    }
    
    # read back in as json    
    value <- data.frame(
      "_id" = ifelse(
        test = rep(length(idcol), nrow(value)),
        yes = as.character(value[["_id"]]), 
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
