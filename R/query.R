#' Get documents with a filtering query
#'
#' @export
#' @param src source object, result of call to src
#' @param key (character) A key (collection for mongo)
#' @param query various, see Query section below.
#' @param ... Additional named parameters passed on to each package:
#' - CouchDB: passed on to [sofa::db_query()]
#' - Elasticsearch: passed on to [elastic::Search()]
#' - MongoDB: passed on to the `$find` method of \pkg{mongolite}
#'
#' @template deets
#'
#' @section What is expected for each source:
#' - CouchDB: a list, see docs for [sofa::db_query()]
#' - Elasticsearch: query parameters, see [elastic::Search()]; passed to
#' the `query` parameter of `elastic::Search`, thus performs a URI
#' based search where the query is passed in the URI instead of the body.
#' In theory you can instead pass in a JSON or list to the `body`
#' parameter, but if you want to do complicated Elasticsearch queries
#' you may be better of using \pkg{elastic} package directly
#' - MongoDB: a JSON `query` string and optionally other parameters
#' such as `fields`, see \pkg{mongolite}
#' - SQLite: arameter `query`, a JSON string; supported at the moment
#' is only one level of $or or $and operators with
#' $eq, $gt, $gte, $lt, $lte, $ne and $regex as tests.
#' Optionally, `fields` a JSON string of fields to be
#' returned from anywhere in the tree (in dot paths notation).
#'
#' @section Not supported yet:
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
#' Sys.sleep(2)
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
#'
#' # various query combinations:
#' docdb_create(src, "mtcars", mtcars)
#' docdb_query(src, "mtcars", query = '{"mpg":21}')
#' docdb_query(src, "mtcars", query = '{"mpg":21}', fields = '{"mpg":1, "cyl":1}')
#' docdb_query(src, "mtcars", query = '{"_id": {"$regex": "^.+0.*$"}}', fields = '{"gear": 1}')
#'
#' # regular expressions except for [...] can be used to select fields:
#' docdb_create(src, "mapdata", value = data.frame(mapdata, stringsAsFactors = FALSE))
#' docdb_query(src, "mapdata", fields = '{"rows.elements.d.*text": 1}', query = '{}')
#' docdb_query(src, "mapdata", fields = '{"rows.elements.\\\\S+.text": 1}', query = '{}')
#' docdb_query(src, "mapdata", fields = '{"rows.elements.*somevalue": 1}', query = '{}')
#' }
docdb_query <- function(src, key, query, ...) {
  UseMethod("docdb_query")
}

#' @export
docdb_query.default <- function(src, key, query, ...) {
  stop("docdb_query supported for CouchDB, Elasticsearch, ",
       "MongoDB & SQLite (partially)")
}

#' @export
docdb_query.src_couchdb <- function(src, key, query, ...) {
  assert(key, "character")
  dropmeta(makedf(
    sofa::db_query(src$con, dbname = key,
                   selector = query, limit = 10, ...)$docs))
}

#' @export
docdb_query.src_elastic <- function(src, key, query, ...) {
  assert(key, "character")
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
  jsonlite::flatten(tmp, recursive = TRUE)

}

#' @export
docdb_query.src_sqlite <- function(src, key, query, ...) {

  assert(key, "character")
  assert(query, "character")

  # make dotted parameters accessible
  dotList <- list(...)

  ## special case: return all fields if listfields != NULL
  if (!is.null(dotList$listfields)) {

    # get all fullkeys and types
    fields <- DBI::dbGetQuery(
      conn = src$con,
      statement = paste0(
        "SELECT DISTINCT fullkey
           FROM \"", key, "\", json_tree (\"", key, "\".json);"
      ))[, 1, drop = TRUE]

    # from field names, remove "$." and array elements "$.item[0]"
    fields <- unique(gsub("\\$[.]?|\\[[-#0-9]+\\]", "", fields))
    fields <- sort(fields[fields != ""])

    # return field names
    return(fields)
  }

  ## add limit if not in ...
  n <- -1L
  if (!is.null(dotList$limit)) n <- dotList$limit

  ## handle parameter fields
  fields <- "{}"
  if (!is.null(dotList$fields)) fields <- dotList$fields
  fields <- json2fieldsSql(fields)
  fields <- unique(fields)

  # exclude _id and empty strings from fields
  fields <- fields[fields != "_id"]
  fields <- fields[fields != ""]

  ## special case: early return
  if (!length(fields) && query == "{}") {
    statement <- paste0(
      "  SELECT DISTINCT _id",
      "  FROM \"", key, "\"")
    out <- DBI::dbGetQuery(
      conn = src$con,
      n = n,
      statement = statement)
    return(out)
  }

  # mangle item names into SQL e.g.,
  # "location[4].facility[#-2].name" # two arrayIndex items
  # "location.facil[a-z0-0]+.*thing" # user regexp
  # "location.facil.*"               # user regexp
  fieldsSql <- fieldsSql2fullKey(fields)

  # convert query from json to sql
  querySql <- json2querySql(query, src$con)

  # construct query
  statement <- paste0(
    "  SELECT _id,\n",
    "    CAST (value AS text) AS value,\n",
    "    type,\n",
    "    fullkey\n",
    "  FROM \"", key, "\", json_tree(\"", key, "\".json)")

  # construct query
  statement <- paste0(
    statement,
    "\n  WHERE \n  (",
    #
    # query specified?
    ifelse(
      test = any(querySql != ""),
      yes = paste0(
        "\n   _id IN\n   (\n",
        "    SELECT _id FROM \"", key, "\", json_tree(\"", key, "\".json)\n",
        "    GROUP BY _id\n",
        "    HAVING\n    (",
        paste0(sapply(
          querySql,
          function(x)
            paste0(
              # extra handling of _id, which is not in json
              ifelse(
                test = x[1] == "\"_id\"",
                yes = paste0(" _id ", x[2], " "),
                # to use the correct field as criterion,
                # require the fullkey to be specified
                no = paste0("\n    SUM(fullkey REGEXP \"",
                            gsub("\"", "", fieldsSql2fullKey(x[1])),
                            "\" AND value ", x[2], ") > 0 ")
              )
            ),
          USE.NAMES = FALSE),
          collapse = attr(x = querySql, which = "op")),
        "\n    )\n   )"),
      no = ""),
    #
    # add AND when query and fields present
    ifelse(
      test = any(querySql != "") & any(fieldsSql != ""),
      yes = "\n   AND\n",
      no = ""
    ),
    #
    # fields specified?
    ifelse(
      test = any(fieldsSql != ""),
      yes = paste0(
        "   (",
        paste0(unname(sapply(
          fieldsSql,
          function(x)
            paste0(
              "\n    fullkey REGEXP \"", x, "\""
            ))
        ),
        collapse = " OR "),
        "\n   )"),
      no = ""),
    #
    # finish up
    "\n  );")

  # message(statement)

  # minify statement
  statement <- gsub("[\n\t ]+", " ", statement)

  # adapt user regexp's such as \\S
  statement <- gsub("\\\\", "\\", statement, fixed = TRUE)

  ## do query
  out <- DBI::dbGetQuery(
    conn = src$con,
    n = n,
    statement = statement)

  ## special case: early return
  if (!length(fields)) {
    out <- unique(out[, "_id", drop = FALSE])
    row.names(out) <- NULL
    return(out)
  }

  ## if no rows: early return
  if (!nrow(out)) {
    # empty data frame
    out <- matrix(ncol = length(fields) + 1L, nrow = 0)
    out <- stats::setNames(data.frame(out), c("_id", fields))
    return(out)
  }

  # mangle fullkeys, e.g., $.tags[3]
  out$fullkey <- gsub("^[$][.]", "", out$fullkey)
  out$fullkey <- gsub("\\[[0-9]+\\]", "", out$fullkey)
  out$fullkey <- gsub("[.][0-9]+[.]", ".", out$fullkey)

  # store type and fullkey
  typing <- stats::na.omit(unique(out[, c("fullkey", "type")]))

  # handle situation when for same mangled fullkey,
  # there is more than one type, e.g. array and object:
  # here use the alphabetically first type
  if (nrow(typing)) typing <- stats::aggregate(type ~ fullkey, typing, min)

  # convert from long to wide
  out <- data.table::dcast(
    data.table::setDT(out),
    `_id` ~ fullkey,
    fill = NULL,
    fun.aggregate = list
  )

  # back to data frame
  out <- data.table::setDF(out)

  # remove empty column resulting from NA in fullkey
  out <- out[, !names(out) == "NA", drop = FALSE]

  # add columns for fields for which no values were retrieved
  vn <- names(out)[-1]
  missingFields <- unlist(sapply(
    fields,
    function(f) if (!length(vn[
      grepl(gsub("\\[[-#0-9]+\\][.]", ".",
                 gsub("\\\\", "\\", f, fixed = TRUE)), vn)])) f,
    simplify = TRUE, USE.NAMES = FALSE))
  if (length(missingFields)) out <-
    cbind(out, stats::setNames(
      data.frame(matrix(
        data = NA, nrow = nrow(out), ncol = length(missingFields))),
      nm = missingFields))

  # type columns as per json_tree
  if (nrow(typing)) {
    for (r in seq_len(nrow(typing))) {
      # iterate over relevant out columns
      out[[typing[["fullkey"]][r]]] <-
        switch(
          typing[["type"]][r],
          "integer" = trans2num(out[[typing[["fullkey"]][r]]]),
          "text" = trans2str(out[[typing[["fullkey"]][r]]]),
          "array" = trans2df(out[[typing[["fullkey"]][r]]]),
          "object" = trans2df(out[[typing[["fullkey"]][r]]]),
          out[[typing[["fullkey"]][r]]]
        )
    } # for
  } # if

  # return
  return(out)
}


## helpers --------------------------------------


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
  # - check if "AND" operation is specified
  if (grepl(pattern = '^[{]"[$]and":', x = x)) {
    # remove outer brackets
    x <- gsub(pattern = '^[{]"[$]and":[[{]', replacement = "", x = x)
    x <- gsub(pattern = "][}]$", replacement = "", x = x)
    op <- "AND"
  }
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
  x <- gsub(pattern = "[$]regex:", replacement = "REGEXP ", x = x)

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

# generate regular expressions for
# fields so that fullkey matches
fieldsSql2fullKey <- function(x) {

  # check e.g. if only fields = '{"_id": 1}'
  if (!length(x)) return("")

  # remove array indices from fields
  x <- gsub("\\[[-#0-9]+\\][.]", ".", x)

  # protect "." between item and subitem using lookahead for overlapping groups
  x <- gsub("([a-zA-Z]+)[.](?=[a-zA-Z]+)", "\\1@@@\\2", x, perl = TRUE)

  # add in regexps to match any arrayIndex in fullkey
  x <- paste0("^[$][.]", gsub("@@@", "[-#\\\\[\\\\]0-9]*[.]", x), "$")

  # return
  return(x)
}

# for transform column types
# - from character to num
trans2num <- function(clmn) {
  o <- lapply(
    clmn,
    function(r) {
      r <- as.numeric(r)
      if (!length(r)) {NA} else {r}
    })
  if (all(vapply(o, length, numeric(1)) == 1L)) {
    o <- unlist(o, recursive = FALSE)
  }
  o
}
# - from character to list of character
trans2str <- function(clmn) {
  o <- lapply(
    clmn,
    function(r) {
      r <- as.character(r)
      if (!length(r)) {NA} else {r}
    })
  if (all(vapply(o, length, numeric(1)) == 1L)) {
    o <- unlist(o, recursive = FALSE)
  }
  o
}
# - helper
json2data <- function(x, ...) {
  out <- try(
    # this already validates
    jsonlite::fromJSON(x, ...),
    silent = TRUE)
  if (inherits(out, "try-error")) return(x)
  return(out)
}
# - from json to data frame
trans2df <- function(clmn) {
  sapply(
    clmn,
    function(r) {
      if (!length(r) || all(is.na(r))) {NA} else {
        tmp <- sapply(r, function(i)
          unlist(i, use.names = FALSE),
          USE.NAMES = FALSE)
        sapply(tmp, json2data, USE.NAMES = FALSE,
               flatten = FALSE, simplify = FALSE)
      }
    }, USE.NAMES = TRUE, simplify = TRUE)
}
