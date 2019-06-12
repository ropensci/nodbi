#' Update documents
#'
#' @export
#' @param src source object, result of call to src
#' @param key (chartacter) A key. ignored for mongo
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





