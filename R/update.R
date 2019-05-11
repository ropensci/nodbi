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
  
  assert(key, "character") # table name
  assert(value, 'data.frame') # two columns for path, value (key is name of value column)
  
  if (dim(value)[2] != 2) 
    stop("value must have exactly 2 columns (path, value), ",
         "with names corresponding to the respective fields")
  
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
                        value[i, 2], 
                        stringsAsFactors = FALSE,
                        check.rows = FALSE)
      
      names(tmp) <- c("_id", vn[2])
      
      tmp
      
    })
    
    # make new value data frame
    value <- do.call(rbind, value)
    
  }
  
  # identifier is table index
  nrowaffected <- sapply(seq_len(nrow(value)), function(i) {
    
    DBI::dbExecute(
      conn = src$con, 
      statement = sprintf(
        "UPDATE %s
         SET json = 
          (SELECT json_set( 
                  json( %s.json ), '$.%s', json ( %s ))
           FROM %s 
           WHERE _id = '%s')
         WHERE _id = '%s';", 
        key, 
        key, vn[2], value[i, 2], 
        key,
        value[i, 1],
        value[i, 1]
      ))
  })
  
  # TODO: remove
  # gsub("[ \n]+", " ", statement)
  
  invisible(sum(nrowaffected, na.rm = TRUE))
  
}
