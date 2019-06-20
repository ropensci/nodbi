#' Update documents
#'
#' @export
#' @param src source object, result of call to src
#' @param key (chartacter) A key. ignored for mongo
#' @param value (data.frame) A single data.frame
#' @param ... Ignored
#' @details Only CouchDB supported for now
#' @examples \dontrun{
#' # CouchDB
#' src <- src_couchdb()
#' docdb_create(src, "mtcars2", mtcars)
#' docdb_get(src, "mtcars2")
#'
#' mtcars$letter <- sample(letters, NROW(mtcars), replace = TRUE)
#' invisible(docdb_update(src, "mtcars2", mtcars))
#' docdb_get(src, "mtcars2")
#' }
docdb_update <- function(src, key, value, ...) {
  UseMethod("docdb_update")
}

#' @export
docdb_update.src_couchdb <- function(src, key, value, ...) {
  assert(value, 'data.frame')
  if (!key %in% attr(src, "dbs")) sofa::db_create(src[[1]], dbname = key)
  sofa::db_bulk_update(src[[1]], dbname = key, doc = value, ...)
}

#' @export
docdb_update.src_mongo <- function(src, key, value, ...) {
  
  # check expectations
  if (exists("key", inherits = FALSE) && 
      src$collection != key) 
    message("Parameter 'key' is different from parameter 'collection', ",
            "was given as ", src$collection, " in src_mongo().")
  
  # The aim is to use this method: 
  # mongolite::mongo()$update(query, update = '{"$set":{}}', upsert = FALSE, multiple = FALSE)
  # It is necessary to define: 
  # - which documents to update (based on parameter "query" if specified, 
  #   or column _id in dataframe value, or all documents in collection)
  # - what to use for updating (the dataframe value will be converted 
  #   into a set of changes, where column names indicate what element
  #   in the document to update, and will be replaced with non-NA values 
  #   in the dataframe for the respective document)

  assert(value, 'data.frame')

  # Get ellipsis
  dotparams <- list(...)
  
  # Which documents to update? 
  # - Check if query is specified
  query <- rep.int(x = ifelse(is.null(dotparams$query), "{}", dotparams$query), 
                   times = nrow(value))
  
  # - Is _id in dataframe value?
  if (all(query != "{}") & any(grepl("_id", names(value)))) {
    stop("Specify only one of query = '...' or '_id' as column in data frame 'value'.")
  }
  
  # - Get query from dataframe value
  if (any(grepl("_id", names(value)))) {
    # if dataframe has has a column _id, 
    # turn it into query and then remove it
    query <- paste0('{"_id":"', value[["_id"]], '"}')
    value <- value[, -match("_id", names(value)), drop = FALSE]
  } else {
    # if dataframe has at least two columns, 
    # use FIRST for query and remove it
    if (ncol(value) >= 2L) {
      quoting <- ifelse(class(value[, 1]) == "character", "\"", "")
      query <- paste0('{"', names(value)[1], '":{"$eq":', quoting, value[, 1, drop = TRUE], quoting, '}}')
      query <- paste0('{"', names(value)[1], '":', quoting, value[, 1, drop = TRUE], quoting, '}')
      value <- value[, -1, drop = FALSE]
    }
  }
  
  # What to use for update`
  # - Convert dataframe value's rows into vector of 
  #   json sets. Note: NAs are removed by toJSON. 
  #   No conversion if already json string. 
  value <- sapply(X = seq_len(nrow(value)), 
                  FUN = function(i) 
                    ifelse(!is.character(value[i,]) || 
                           !jsonlite::validate(value[i,]),
                           jsonlite::toJSON(x = value[i, , drop = FALSE], 
                                            dataframe = "rows",
                                            auto_unbox = TRUE), 
                           value[i,]))
  
  # - Remove outer []
  value <- gsub(pattern = "^\\[|\\]$", 
               replacement = "",
               x = value)
  
  # - Turn into json set
  value <- paste0('{"$set":', value, '}')
  
  # - To update, iterate over json set vector
  nrowaffected <-
    sapply(seq_len(length(value)), 
           function(i) 
             # update(query, update = '{"$set":{}}', upsert = FALSE, multiple = FALSE)
             src$con$update(query = query[i],  # which documents?
                            update = value[i], # with what to update? 
                            upsert = TRUE,     # ok to add new documents
                            multiple = TRUE)   # ok to update several documents
    )
  
  # Extract number of modified or added documents
  #               [,1]         
  # modifiedCount 0            
  # matchedCount  0            
  # upsertedCount 1            
  # upsertedId    "NCT00097292"
  nrowaffected <- data.frame(nrowaffected, 
                             stringsAsFactors = FALSE)[c(1, 3), , drop = TRUE]
  
  invisible(sum(unlist(nrowaffected), na.rm = TRUE))
  
}
