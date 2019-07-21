#' Update documents
#'
#' @export
#' @param src source object, result of call to src
#' @param key (character) A key (collection for mongo)
#' @param value (data.frame) A single data.frame
#' @param ... Ignored
#' @details Only CouchDB and sqlite supported for now
#' @examples \dontrun{
#' # CouchDB
#' src <- src_couchdb()
#' docdb_create(src, "mtcars2", mtcars)
#' docdb_get(src, "mtcars2")
#'
#' mtcars$letter <- sample(letters, NROW(mtcars), replace = TRUE)
#' invisible(docdb_update(src, "mtcars2", mtcars))
#' docdb_get(src, "mtcars2")
#' 
#' # SQLite
#' src <- src_sqlite()
#' docdb_create(src, "mtcars",
#'              data.frame("_id" = seq_len(nrow(mtcars)),
#'                         mtcars,
#'                         stringsAsFactors = FALSE,
#'                         check.names = FALSE))
#' dfupd <- data.frame("cyl" = 4, "gear" = 99,
#'                     stringsAsFactors = FALSE,
#'                     check.names = FALSE)
#' docdb_update(src, "mtcars", dfupd)
#' docdb_query(src, "mtcars",
#'             query = '{"gear": {"$gt": 10}}',
#'             fields = '{"gear": 1, "cyl": 1}')
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

#' @export                  
docdb_update.src_sqlite <- function(src, key, value, ...) {
  
  assert(key, "character")
  assert(value, 'data.frame') 
  
  # Two columns for path, value ("key" is name of value column)

  # If table does not exist, create empty table
  if (!docdb_exists(src = src, key = key)) {
    docdb_create(src = src, key = key, value = NULL)
  }
  
  # https://www.sqlite.org/json1.html#jrepl
  # json_set() 
  #  - Overwrite if already exists?	Yes. 
  #  - Create if does not exist? Yes.
  # first argument: single JSON value
  # second argument: zero or more pairs of path and value
  # returns: new JSON string formed by updating the input JSON by path/value pairs
  # emulate this:
  # json_set('{"a":2,"c":4}', '$.c', json('[97,96]')) → '{"a":2,"c":[97,96]}'
  
  vn <- names(value)
  
  if ("_id" != vn[1]) {
    
    # get _id of records
    idsaffected <- sapply(seq_len(nrow(value)), function(i) {
      
      DBI::dbGetQuery(conn = src$con, 
                      statement = sprintf(
                        "SELECT _id
                              FROM %s, json_each( %s.json )
                              WHERE key = '%s'
                                AND value = %s;",
                        key, key,
                        vn[1], value[i , 1]))
    })
    
    # replace first column with _id
    value <- lapply(seq_len(nrow(value)), function(i) {
      
      tmp <- data.frame(idsaffected[i], 
                        value[i, -1], 
                        stringsAsFactors = FALSE,
                        check.rows = FALSE)
      
      names(tmp) <- c("_id", vn[-1])
      
      tmp
      
    })
    
    # make new value data frame
    value <- do.call(rbind, value)
    
  }
  
  # iterate over columns
  ncoliterated <- sapply(seq(2, ncol(value)), function(i) {
    
    # identifier _id is table index
    nrowaffected <- 
      sapply(seq_len(nrow(value)), function(ii) {
        
        statement <- sprintf(
          "UPDATE %s
         SET json = 
          (SELECT json_set( 
                  json( %s.json ), '$.%s', json ( %s ))
           FROM %s 
           WHERE _id = '%s')
         WHERE _id = '%s';", 
          key, 
          key, vn[i], valueEscape(value[ii, i, drop = TRUE]), 
          key,
          value[ii, 1],
          value[ii, 1]
        )
        
        # execute
        DBI::dbExecute(
          conn = src$con, 
          statement = statement)
        
      }) # nrowaffected
    nrowaffected
  })
  
  invisible(sum(ncoliterated, na.rm = TRUE))
  
}


## helpers --------------------------------------

valueEscape <- function(x) {

  # cf. https://www.sqlite.org/json1.html#jset  
  switch(class(x),
         
         # - character: '"stringvalue"'
         "character" = ifelse(test = grepl("^[{].*[}]$", x), 
                              yes = paste0('\'', x, '\''), 
                              no = paste0('\'\"', x, '\"\'')),
         # - list e.g.: '{"a": "something", "b": 2}'
         "list" = paste0('\'', jsonlite::toJSON(x), '\''),
         # - no quotation for integers, real
         "numeric" = paste0(x),
         # - default, all others: 'value'
         paste0('\'', x, '\'')
  )
  
}

