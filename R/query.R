#' Get documents with a filtering query
#'
#' @export
#' @param src source object, result of call to src
#' @param key (character) A key (collection for mongo)
#' @param query various. see Query section below.
#' @param ... Additional named parameters passed on to each package:
#' 
#' - CouchDB: passed on to [sofa::db_query()]
#' - Elasticsearch: passed on to [elastic::Search()]
#' - MongoDB: passed on to the `$find` method of \pkg{mongolite}
#' 
#' @template deets
#' @section What is expected for each source:
#' 
#' - CouchDB: a list, see docs for [sofa::db_query()]
#' - Elasticsearch: query parameters, see [elastic::Search()]; passed to 
#' the `query` parameter of `elastic::Search`, thus performs a URI 
#' based search where the query is passed in the URI instead of the body.
#' In theory you can instead pass in a JSON or list to the `body`
#' parameter, but if you want to do complicated Elasticsearch queries
#' you may be better of using \pkg{elastic} package directly
#' - MongoDB: query parameters, see \pkg{mongolite} docs for 
#' help with searches
#' - SQLite: `fields`, an optional json string of fields to be 
#' returned from anywhere in the tree. 
#' Parameter `query`, a json string In analogy to MongoDB, 
#' a comma separated list of expressions provides an implicit 
#' AND operation. Nested or otherwise complex queries are not
#' yet supported. 
#' 
#' @section Not supported yet:
#' 
#' - Redis
#' 
#' @examples \dontrun{
#' # CouchDB
#' (src <- src_couchdb())
#' if (docdb_exists(src, "mtcars2")) docdb_delete(src, "mtcars2")
#' invisible(docdb_create(src, key = "mtcars2", value = mtcars))
#' docdb_exists(src, "mtcars2")
#' (query <- list(cyl = list("$gt" = 6)))
#' docdb_query(src, "mtcars2", query = query)
#'
#' # Elasticsearch
#' src <- src_elastic()
#' if (docdb_exists(src, "iris")) docdb_delete(src, "iris")
#' docdb_create(src, "iris", iris)
#' docdb_exists(src, "iris")
#' docdb_query(src, "iris", query = "setosa")
#' docdb_query(src, "iris", query = "1.5")
#' docdb_query(src, "iris", query = "Petal.Width:1.5")
#'
#' # Mongo
#' src <- src_mongo(collection = "mtcars")
#' if (docdb_exists(src, "mtcars")) docdb_delete(src, "mtcars")
#' docdb_create(src, "mtcars", mtcars)
#' docdb_query(src, "mtcars", query = '{"mpg":21}')
#' docdb_query(src, "mtcars", query = '{"mpg":21}', fields = '{"mpg":1, "cyl":1}')
#' docdb_get(src, "mtcars")
#' 
#' # SQLite
#' src <- src_sqlite()
#' docdb_create(src, "mtcars", mtcars)
#' docdb_query(src, "mtcars", query = "{}", fields = '{"mpg":1, "cyl":1}')
#' docdb_query(src, "mtcars", query = '{"gear": {"$lte": 4}}', fields = '{"gear": 1}')
#' # for RSQLite from 2.1.2 using PCRE regular expressions
#' docdb_query(src, "mtcars", query = '{"_id": {"$regex": "^.+0.*$"}}', fields = '{"gear": 1}')
#' }
docdb_query <- function(src, key, query, ...){
  UseMethod("docdb_query")
}

#' @export
docdb_query.default <- function(src, key, query, ...) {
  stop("docdb_query supported for CouchDB, Elasticsearch & MongoDB")
}

#' @export
docdb_query.src_couchdb <- function(src, key, query, ...) {
  assert(key, 'character')
  dropmeta(makedf(
    sofa::db_query(src$con, dbname = key,
      selector = query, limit = 10, ...)$docs))
}

#' @export
docdb_query.src_elastic <- function(src, key, query, ...) {
  assert(key, 'character')
  ids <- pluck(elastic::Search(src$con, key, q = query, source = FALSE,
                               size = 10, ...)$hits$hits, "_id", "")
  if (length(ids) == 0) return(data.frame(NULL))
  tmp <- elastic::docs_mget(src$con, index = key, type = key, ids = ids,
                            verbose = FALSE)
  makedf(pluck(tmp$docs, "_source"))
}

#' @export
docdb_query.src_mongo <- function(src, key, query, ...) {
  
  # check expectations
  if (exists("key", inherits = FALSE) && 
      src$collection != key) 
    message("Parameter 'key' is different from parameter 'collection', ",
            "was given as ", src$collection, " in src_mongo().")
  
  # get results
  tmp <- src$con$find(query = query, ...)
  
  # ensure results are flattened
  jsonlite::flatten(tmp)
  
}

#' @export
docdb_query.src_sqlite <- function(src, key, query, ...) {
  
  assert(key, "character")
  assert(query, "character")
  
  # make dotted parameters accessible
  tmpdots <- list(...)
  
  # https://www.sqlite.org/json1.html#jeach
  # need to obtain the types of fields
  tmpstr <- suppressWarnings(
    DBI::dbGetQuery(
      conn = src$con, 
      statement = paste0(
        "SELECT DISTINCT fullkey, type
         FROM ", key, ", json_tree (", key, ".json) AS tt
         WHERE tt.type <> 'object';"
      )))
  
  ## convert parameter fields
  fields <- "{}"
  if (!is.null(tmpdots$fields)) fields <- tmpdots$fields
  tmpfields <- json2fieldsSql(fields)
  
  # if no fields specified, get names without $. prefix
  if (all(tmpfields == ""))
    tmpfields <- unique(gsub("\\$[.]|\\[[0-9]+\\]", "", tmpstr$fullkey))
  
  # exclude _id from fields
  tmpfields <- tmpfields[tmpfields != "_id"]
  
  # special case: return all fields if listfields != NULL
  if (!is.null(tmpdots$listfields)) return(tmpfields)
  
  ## convert parameter query
  if (is.null(query)) query <- "{}"
  tmpquery <- json2querySql(query, src$con)
  
  ## compose statement
  statement <-
    paste0(
      # SELECT '{' || "
      #        ' "gear": ' || json_extract(mtcars.json, '$.gear') || ', ' || 
      #        ' "cyl": '  || json_extract(mtcars.json, '$.cyl')  || ', ' ||
      #        ' "_id": "' || _id || '"}'
      # AS json
      "SELECT DISTINCT '{' || ",
      paste0(
        unname(
          sapply(
            tmpfields, 
            function(x) 
              sprintf("' \"%s\": %s' || json_extract(%s.json, '$.%s') || '%s, ' ||",  
                      x, jsonEscape(tmpstr, x), key,
                      x, jsonEscape(tmpstr, x)
              ))), 
        collapse = "\n")
      , " ' \"_id\": \"' || _id || '\"}'
        AS json ",
      
      # select logical operation
      ifelse(
        attr(x = tmpquery, which = "op") == "OR", 
        
        # "OR" operation
        paste0(
          # FROM mtcars, 
          #      json_tree (mtcars.json) AS tt
          # WHERE
          #    (tt.key = 'cyl'  AND tt.value = 8) 
          # OR (tt.key = 'gear' AND tt.value = 4) 
          "FROM ", key, 
          ifelse( # check if query specified
            tmpquery != "",
            paste0(", json_tree (", key, ".json) AS tt
                    WHERE ", 
                   paste0(
                     paste0(
                       unname(
                         sapply(
                           tmpquery, 
                           function(x) 
                             # extra handling of _id, which is not in json
                             ifelse(x[1] == "\"_id\"", 
                                    # check _id column
                                    paste0(" _id ", x[2], " "),
                                    # check json column
                                    paste0(" (tt.key = ", x[1], " AND tt.value ", x[2], ") "))
                         ))), 
                     collapse = "OR \n")), 
            "") # ifelse empty query
        ), # paste0 OR operation
        
        # "AND" operation
        paste0(
          "FROM ", key, " \n",
          # FROM mtcars
          # WHERE _id IN
          # (
          # SELECT _id 
          #   FROM  mtcars, json_tree (mtcars.json) AS tt
          #   WHERE (tt.key = 'cyl' AND tt.value = 4)
          # INTERSECT
          #  SELECT _id 
          #   FROM  mtcars, json_tree (mtcars.json) AS tt
          #   WHERE (tt.key = 'gear' AND tt.value >= 3)
          # )      
          ifelse( # check if query specified
            tmpquery != "",
            paste0("WHERE _id IN (", 
                   paste0(
                     paste0(
                       unname(
                         sapply(
                           tmpquery, 
                           function(x) 
                             paste0("SELECT _id FROM ", key, 
                                    # extra handling of _id, which is not in json
                                    ifelse(x[1] == "\"_id\"", 
                                           # check _id column
                                           paste0(" AS tt WHERE tt._id ", x[2], " "),
                                           # check json column
                                           paste0(", json_tree (", key, ".json) AS tt ", 
                                                  "WHERE (tt.key = ", x[1], " AND 
                                                   tt.value ", x[2], ") "))
                             ))
                         )), 
                     collapse = "INTERSECT \n"),
                    ")"), 
            "") # ifelse empty query
        )
      ), # ifelse OR or AND operation
      # close sql
      ";")
  
  # minify statement
  statement <- gsub("[\n ]+", " ", statement)
  
  # add limit if not in ...
  n <- -1L
  if (!is.null(tmpdots$limit)) n <- tmpdots$limit
  
  ## do query
  
  # temporary file; note this cannot be 
  # because it is needed to stream 
  # the return value
  dump <- tempfile()
  
  # get data, write to file in ndjson format
  cat(stats::na.omit(unlist(
    DBI::dbGetQuery(conn = src$con,
                    statement = statement, 
                    n = n)
  )
  ),
  sep = "\n", # ndjson
  file = dump)
  
  # from jsonlite:
  # Because parsing huge JSON strings is difficult and inefficient, 
  # JSON streaming is done using lines of minified JSON records, a.k.a. ndjson. 
  jsonlite::stream_in(file(dump), verbose = FALSE)

}


## helpers --------------------------------------


# column type escaping
jsonEscape <- function(x, y) {
  
  # parameters:
  # - x is a data frame created in docdb_query.src_sqlite
  #     with columns fullkey and type as per json_tree()
  # - y is the name of a variable / column of x that is of interest

  # default
  o <- ""
  
  # format like fullkey
  y <- paste0("$.", y)
  
  # no escaping for:
  # - lists, which correspond to arrays
  # - numerics
  
  # do escaping for:
  # - strings
  if (all(x$type[x$fullkey == y] == "text")) o <- "\""
  # - special case, row names
  if (y == "$._row") o <- "\""
  
  return(o)
}


# converst json string into
# string with sql fields
json2fieldsSql <- function(x) {
  
  if (!jsonlite::validate(x)) stop("No json: ", x)
  
  # empty query
  if (x == "{}") return("")
  
  # minify to simplify
  x <- jsonlite::minify(x)
  
  # find numeric 0's and 1's on right hand side
  p <- '["](.+?)["][ ]*:[ ]*["]?[01]["]?'
  
  # construct regular expression
  m <- gregexpr(pattern = p, 
                text = x)
  
  # produce matches
  r <- regmatches(x = x, 
                  m = m, 
                  invert = FALSE)
  
  # keep only 1's
  r <- unlist(r)
  r <- r[grepl("1$", r)]
  
  # replace quotation marks
  x <- gsub('.*["](.+?)["].*', "\\1", r)
  
  # return
  x
  
}
# json2fieldsSql(x = '{"cut" : "1", "price": 1 }') # "cut" not returned because 1 is string
# json2fieldsSql(x = '{"_id": 1, "annotation": 0, "other": 1}') # correctly specified numbers
# json2fieldsSql('{}')


# convert json query string to
# string as part of sql WHERE
json2querySql <- function(x, con) {
  
  if (!jsonlite::validate(x)) stop("No json: ", x)
  
  # replaces 
  # - atomic expression { A : { $op :B } }
  # - simple expressions 

  # standard operation
  op <- "AND"
  
  # empty query
  if (x == "{}") {
    out <- ""
    attr(out, "op") <- op
    return(out)
  }
  
  # minify to simplify
  x <- jsonlite::minify(x)
  
  # main logical operation for concatenated criteria: 
  # - implicit AND, when specifying a comma separated list of expressions
  # - check if "OR" operation is specified
  if (grepl(pattern = '^[{]"[$]or":', x = x)) {
    # remove outer brackets
    x <- gsub(pattern = '^[{]"[$]or":[[{]', replacement = "", x = x)
    x <- gsub(pattern = "][}]$", replacement = "", x = x)
    op <- "OR"
  }
  
  # separate by commata, will later be concatenated using AND
  x <- unlist(strsplit(x = x, split = ","))
  
  # get left hand side and remove special characters
  LHS <- sapply(strsplit(x = x, split = ":"), "[[", 1)
  LHS <- trimws(gsub("[{} ]", "", LHS))
  
  # get right hand side, that is, operator and value
  x <- gsub(pattern = ".*?:(.*)", replacement = "\\1", x = x)
  
  # remote brackets, blanks and quotation marks around operator
  x <- gsub(pattern = "[{}]", replacement = "", x = x)
  x <- gsub(pattern = "[ ]+:[ ]+", replacement = ":", x = x)
  x <- gsub(pattern = "\"([$][a-z]+)\"[ ]*:", replacement = "\\1:", x = x)
  x <- trimws(x)
  
  # special case where no operator, only term
  x <- ifelse(grepl("[:$]", x), x, paste0(" = ", x))
  
  # replace json operators with sql operators
  # https://docs.mongodb.com/manual/reference/operator/query/
  # https://sqlite.org/lang_expr.html#booleanexpr
  x <- gsub(pattern = "[$]eq:", replacement = "= ", x = x)
  x <- gsub(pattern = "[$]gt:", replacement = "> ", x = x)
  x <- gsub(pattern = "[$]gte:", replacement = ">= ", x = x)
  x <- gsub(pattern = "[$]lt:", replacement = "< ", x = x)
  x <- gsub(pattern = "[$]lte:", replacement = "<= ", x = x)
  x <- gsub(pattern = "[$]ne:", replacement = "!= ", x = x)
  
  # special case
  # https://docs.mongodb.com/manual/reference/operator/query/regex/#pcre-vs-javascript
  x <- gsub(pattern = "[$]regex:", 
            replacement = ifelse(attr(x = con, which = "regexp.extension"), 
                                 "REGEXP ", "LIKE "), 
            x = x)

  # make right hand side
  RHS <- x
  
  # concatenate for return
  out <- lapply(seq_along(LHS), function(x) c(LHS[x], RHS[x]))
  
  # add logical operation
  attr(out, "op") <- op
  
  # return
  return(out)
  
}
# json2querySql(x = '{"cut" : "Premium", "price" : { "$lt" : 1000 }, "_id": {"$ne": 4}, "_row": {"$regex": "Merc"} }')
# json2querySql(x = '{ "$or": [ { "gear": { "$lt": 5 } }, { "cyl": 6 } ] }')
# json2querySql(x = "{}")













