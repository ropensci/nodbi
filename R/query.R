#' Get documents or parts with filtering query
#'
#' @inheritParams docdb_create
#'
#' @param query (character) A JSON query string, see examples.
#'  Can use comparisons / tests (e.g., '$gt', '$ne', '$in', '$regex'),
#'  with at most one logic operator ('$and' if not specified, or '$or'),
#'  see examples.
#'
#' @param ... Optionally, specify `fields` as a JSON string of
#' fields to be returned from anywhere in the tree, see examples.
#'
#' @note A dot in `query` or `fields` is interpreted as a dot path;
#' it is not supported to have a dot within the key / name of a field.
#'
#' Main functions used per database:
#' - MongoDB: find() in [mongolite::mongo()]
#' - SQLite: SQL query using built-in `json_tree()`
#' - Elasticsearch: [elastic::Search()]
#' - CouchDB: [sofa::db_query()]
#' - PostgreSQL: SQL query using built-in `jsonb_build_object()`
#' - DuckDB: SQL using built-in `json_extract()`
#'
#' @return Data frame with requested documents, may have nested
#'  lists in columns. If \code{query = "{}"} and fields are not
#'  specified, consider using [docdb_get()].
#'
#' @export
#'
#' @examples \dontrun{
#' src <- src_sqlite()
#' docdb_create(src, "mtcars", mtcars)
#' docdb_query(src, "mtcars", query = '{"mpg":21}')
#' docdb_query(src, "mtcars", query = '{"mpg":21, "gear": {"$lte": 4}}')
#' docdb_query(src, "mtcars", query = '{"mpg":21}', fields = '{"_id":0, "mpg":1, "cyl":1}')
#' docdb_query(src, "mtcars", query = '{"_id": {"$regex": "^.+0.*$"}}', fields = '{"gear": 1}')
#' # complex query, not supported for src_elastic and src_couchdb backends at this time:
#' docdb_query(src, "mtcars", query = '{"$and": [{"mpg": {"$lte": 18}}, {"gear": {"$gt": 3}}]}')
#' }
docdb_query <- function(src, key, query, ...) {
  assert(src, "docdb_src")
  assert(key, "character")
  assert(query, "character")
  UseMethod("docdb_query", src)
}

#' @export
docdb_query.src_couchdb <- function(src, key, query, ...) {

  # https://cran.r-project.org/web/packages/sofa/vignettes/query_tutorial.html

  # TODO refactor to use jqr in analogy to function dbiGetProcessData

  # make dotted parameters accessible
  limit <- 9999999L
  params <- list(...)
  if (!is.null(params[["limit"]])) {
    limit <- params$limit
    params$limit <- NULL
  }

  # for fields, change from MongoDB to couchdb syntax
  tmpFields <- ""
  if (length(params[["fields"]])) {
    tmpFields <- params[["fields"]]
    m <- stringi::stri_match_all_regex(params[["fields"]], '"([-@._\\w]+?)":[ ]*1')[[1]][, 2, drop = TRUE]
    if (!is.na(m[1])) fields <- m
    params[["fields"]] <- NULL
    if (!is.na(m[1]) && length(fields)) {
      # cannot filter for subitems in documents, thus keep only their roots
      if (any(grepl("[.]", fields))) message(
        "Note: return root field(s) because subfields cannot be accessed for: ",
        paste0(fields[grepl("[.]", fields)], collapse = ", "))
      fields <- sub("(.+?)[.].*", "\\1", fields)
      fields <- paste0('"fields": [', paste0('"', fields, '"', collapse = ", "), ']')
      # ensure only records are returned that have sought fields
      if (query == '{}') {
        addQuery <- paste0("{", paste0('"', sub("(.+?)[.].*", "\\1", m), '":{"$exists":true}'), "}", collapse = ",")
        query <- paste0('{"$and":[', addQuery, ']}')
      }
    }
  }

  # mangle query and fields
  # - remove any regexp options
  query <- sub(',? *?"[$]options" *: *".*?"', " ", query)
  # - add selector
  query <- paste0('"selector": ', query)
  if (exists("fields", inherits = FALSE)) {
    query <- paste0(query, ", ", fields)
  }
  # - add limit
  query <- paste0(
    '{', query, ', "limit": ', limit, '}')

  # get data
  out <- jsonlite::fromJSON(
    do.call(
      sofa::db_query,
      c(list(
        cushion = src$con,
        dbname = key,
        query = query,
        as = "json"),
        params)
    ))[["docs"]]

  # safeguard against empty out
  if (length(out)) {
    # remove any _rev column
    rM <- stats::na.omit(match("_rev", names(out)))
    if (length(rM)) out <- out[, -rM, drop = FALSE]
    rm <- NULL
    # remove any column with field_name: 0
    m <- stringi::stri_match_all_regex(tmpFields, '"([-@._\\w]+?)":[ ]*0')[[1]][, 2, drop = TRUE]
    if (!is.na(m[1])) tmpFields <- m
    rM <- stats::na.omit(match(tmpFields, names(out)))
    if (length(rM)) out <- out[, -rM, drop = FALSE]
  }

  # return
  return(out)

}

#' @export
docdb_query.src_elastic <- function(src, key, query, ...) {

  # https://www.elastic.co/guide/en/elasticsearch/reference/current/query-dsl.html

  # TODO refactor to use jqr in analogy to function dbiGetProcessData

  # make dotted parameters accessible
  params <- list(...)
  if (!is.null(params[["limit"]])) limit <- params$limit
  if (is.null(params[["limit"]])) limit <- 10000L
  # for fields, change from MongoDB to couchdb syntax
  if (length(params[["fields"]])) {
    #
    m <- stringi::stri_match_all_regex(params[["fields"]], '"([-@._\\w]+?)":[ ]*1')[[1]][, 2, drop = TRUE]
    if (!is.na(m[1])) fields <- m
    if (!is.na(m[1]) && length(fields)) {
      # cannot filter for subitems in documents, thus keep only their roots
      if (any(grepl("[.]", fields))) message(
        "Note: return root field(s) because subfields cannot be accessed for: ",
        paste0(fields[grepl("[.]", fields)], collapse = ", "))
      params[["source_includes"]] <- unique(gsub("(.+?)[.].*", "\\1", fields))
    }
    #
    m <- stringi::stri_match_all_regex(params[["fields"]], '"([-@._\\w]+?)":[ ]*0')[[1]][, 2, drop = TRUE]
    if (!is.na(m[1])) fields <- m
    if (!is.na(m[1]) && length(fields)) params[["source_excludes"]] <- fields
    params[["fields"]] <- NULL
  }

  # simple query mapping to Elasticsearch
  query <- paste0('{"query": {"match": ', query, '}}')

  # get _id's
  docids <- do.call(
    elastic::Search,
    args = c(src$con, key, source = FALSE,
             size = limit, body = query, params)
  )[["hits"]][["hits"]]

  # check for empty index
  if (!length(docids)) return(NULL)

  # keep only those with score about 1
  docids <- docids[sapply(docids, "[[", "_score") >= 1.0]

  # extract docids
  docids <- sapply(docids, "[[", "_id", USE.NAMES = FALSE, simplify = TRUE)
  docids <- sort(docids)

  # check for empty index
  if (is.null(docids)) return(NULL)

  # get results
  if (length(docids) == 1L) {
    # single document
    result <- do.call(
      elastic::docs_get,
      args = c(conn = src$con, index = key, id = docids[1],
               verbose = FALSE, params))
    if (length(result[["_source"]])) {
      result <- c(result[["_id"]], result[["_source"]])
      result <- jsonlite::fromJSON(jsonlite::toJSON(result, auto_unbox = TRUE))
      result <- data.frame(t(result), stringsAsFactors = FALSE, check.names = FALSE)
    } else {
      # this is called when no params[["source_includes"]] were found
      result <- data.frame("_id" = result[["_id"]], stringsAsFactors = FALSE, check.names = FALSE)
    }
  } else {
    # multiple documents
    result <- elastic::docs_mget(
      conn = src$con, index = key, ids = docids, verbose = FALSE, type = NULL,
      type_id = NULL, index_type_id = NULL, source = NULL, fields = NULL,
      raw = FALSE, callopts = list())[["docs"]]
    result <- lapply(result, function(i) c("_id" = i[["_id"]], i[["_source"]]))
    result <- jsonlite::fromJSON(jsonlite::toJSON(result, auto_unbox = TRUE))
    #
    # implement field selection
    srcNames <- NULL
    if (!is.null(params[["source_includes"]])) srcNames <-
      stats::na.omit(match(params[["source_includes"]], names(result)))
    if (length(srcNames)) result <- result[, srcNames, drop = FALSE]
    srcNames <- NULL
    if (is.null(params[["source_includes"]]) &&
        !is.null(params[["source_excludes"]])) srcNames <-
      stats::na.omit(match(params[["source_excludes"]], names(result)))
    if (length(srcNames) && !all(is.na(srcNames))) result <-
      result[, -srcNames, drop = FALSE]
  }

  # output
  return(result)

}

#' @export
docdb_query.src_mongo <- function(src, key, query, ...) {

  chkSrcMongo(src, key)

  # make dotted parameters accessible
  params <- list(...)

  # canonical sorting in nodbi
  if (!length(params[["sort"]])) params[["sort"]] <- '{"_id": 1}'
  if (!length(params[["fields"]])) params[["fields"]] <- '{}'

  # if regexp query lacks options, add them in
  if (grepl('"[$]regex" *: *"[^,$:}]+?" *}', query)) query <-
    sub('("[$]regex" *: *"[^,$:}]+?" *)', '\\1, "$options": ""', query)

  # make sure fields in query exist in records
  # purpose: match with sqlite, postgres, duckdb
  if (query != '{}') {
    m <- stringi::stri_match_all_regex(query, '"([-@._\\w]+?)":')[[1]][, 2, drop = TRUE]
    addQuery <- paste0("{", paste0('"', m, '":{"$exists":true}'), "}", collapse = ",")
    query <- paste0('{"$and":[', sub("", "", query), ",", addQuery, ']}')
    # jsonlite::validate(query)
  }

  # separate fields to keep and to remove
  if (length(params[["fields"]])) {

    tmpFields <- params[["fields"]]
    params[["fields"]] <- '{}'

    m <- stringi::stri_match_all_regex(tmpFields, '"([-@._\\w]+?)":[ ]*1')[[1]][, 2, drop = TRUE]
    if (!is.na(m[1])) fields <- m
    if (!is.na(m[1]) && length(fields)) params[["fields"]] <-
      paste0('{', paste0('"', fields, '":1', collapse = ','), '}', collapse = '')
    fields <- NULL

    m <- stringi::stri_match_all_regex(tmpFields, '"([-@._\\w]+?)":[ ]*0')[[1]][, 2, drop = TRUE]
    if (!is.na(m[1])) tmpFields <- m
    if (!is.na(m[1]) && length(tmpFields)) fields <- tmpFields

    # make sure sought fields exist but only if no query is specified
    # purpose: match with sqlite, postgres, duckdb
    if (query == '{}') query <- gsub(":1", ':{"$exists":true}', params[["fields"]])

  }

  # get data
  out <- do.call(
    src$con$find,
    c(list(query = query),
      params))

  # handle _id which is always returned even if not in param[["fields]]
  if (!params[["fields"]] == '{}' &&
      !grepl("_id", params[["fields"]])) fields <- unique(c(fields, "_id"))

  # Error: Cannot do inclusion on field name in exclusion projection
  # remove any fields with field_name: 0
  if (length(out)) {
    rM <- stats::na.omit(match(fields, names(out)))
    if (length(rM)) out <- out[, -rM, drop = FALSE]
  }

  # remove rownames
  return(`rownames<-`(out, NULL))

}

#' @export
docdb_query.src_sqlite <- function(src, key, query, ...) {

  # make dotted parameters accessible
  params <- list(...)

  ## special case: return all fields if listfields != NULL
  if (!is.null(params$listfields)) {

    # get all fullkeys and types
    fields <- DBI::dbGetQuery(
      conn = src$con,
      statement = paste0(
        "SELECT DISTINCT REPLACE(fullkey, '\"', '')
         FROM \"", key, "\", json_tree(\"", key, "\".json)
         ;"
      ))[, 1, drop = TRUE]

    # from field names, remove "$." and array elements "$.item[0]"
    fields <- unique(gsub("\\$[.]?|\\[[-#0-9]+\\]", "", fields))
    fields <- sort(fields[fields != ""])

    # return field names
    return(fields)
  }

  ## add limit if not in params
  n <- -1L
  if (!is.null(params$limit)) n <- params$limit

  ## handle fields
  # all following is to emulate mongodb behaviour, which includes avoiding
  # path collisions with overlapping fields; see https://docs.mongodb.com/
  # manual/release-notes/4.4-compatibility/#path-collision-restrictions
  fields <- "{}"
  if (!is.null(params$fields)) fields <- params$fields
  fields <- json2fieldsSql(fields)

  ## special case: early return if only _id is requested
  if (query == "{}" && length(fields) == 1L && fields == "_id") {
    statement <- paste0(
      "  SELECT DISTINCT _id",
      "  FROM \"", key, "\"")
    out <- DBI::dbGetQuery(
      conn = src$con,
      n = n,
      statement = statement)
    return(out)
  }

  ## mangle fields for sql and jq
  # - requested root fields
  rootFields <- fields[!grepl("[.]", fields)]
  # - requested subfields
  subFields <- strsplit(fields[grepl("[.]", fields)], split = "[.]")
  #   keep only subFields that are not in rootFields
  subFields <- subFields[is.na(match(sapply(subFields, "[[", 1), rootFields))]

  ## convert from json to sql
  querySql <- json2querySql(query)

  ## construct sql query statement
  # head of sql
  # -1- no fields specified, get full documents
  if (length(fields) == 1L && fields == "") {
    statement <- paste0(
      # json_tree needed for SQL WHERE below
      "SELECT '{\"_id\":\"' || _id || '\",' || LTRIM(value, '{')
       AS json FROM \"", key, "\", json_tree(\"", key, "\".json)")
  }
  # -2- only _id is specified
  if (length(fields) == 1L && fields == "_id") {
    statement <- paste0(
      "SELECT '{\"_id\": \"' || _id || '\"}' AS json FROM \"", key, "\" ")
  }
  # -3- other fields specified
  if (length(fields[fields != "_id" & fields != ""]) >= 1L) {
    statement <- paste0(
      "SELECT '{\"_id\":\"' || _id || '\",' || group_concat('\"' ||
       LTRIM(REPLACE(fullkey, '\"', ''), '$.') || '\":' ||
       IIF(type = 'text', '\"', '') ||
       IIF(type = 'text', REPLACE(value, '\"', '\\\"'), value) ||
       IIF(type = 'text', '\"', ''))
       || '}' AS json FROM \"", key, "\", json_tree(\"", key, "\".json)")
  }

  # sql where for identifying documents
  # and for identifying json_tree() rows
  statement <- paste0(
    statement,
    " WHERE (",
    ifelse(
      # -1- query identifies _id's of documents
      test = any(querySql != ""),
      yes = paste0(
        " _id IN ( SELECT _id FROM \"",
        key, "\", json_tree(\"", key, "\".json) ",
        "GROUP BY _id HAVING (",
        paste0(
          # handle _id in query, since it is not in json
          sapply(querySql, function(x) paste0(
            ifelse(
              test = x[1] == "\"_id\"",
              yes = paste0(" _id ", x[2], " "),
              # fieldsSql2fullKey has regexps for subfields, arrays
              no = paste0(
                " SUM ( REPLACE(fullkey, '\"', '') REGEXP \"",
                gsub("\"", "", fieldsSql2fullKey(x[1])),
                "\" AND value ", x[2], " ) > 0 ")
            )), USE.NAMES = FALSE),
          # attr contains the logical operator
          collapse = attr(x = querySql, which = "op")),
        ") ) "),
      no = ""),

    # -2- fields identify rows with relevant content
    ifelse(
      test = (length(fields) == 1L) && fields == "_id",
      yes = "",
      no = paste0(
        ifelse(any(querySql != ""), " AND ", ""),
        " REPLACE(fullkey, '\"', '') REGEXP \"",
        ifelse(
          test = (length(fields) == 1L) && fields == "",
          # no field: get root of document
          yes = "[$]",
          # get fields but only rootFields and roots of subFields
          no = paste0("^(", paste0(
            fieldsSql2fullKey(unique(c(
              rootFields[rootFields != "_id"],
              sapply(subFields, "[[", 1)))),
            collapse = '|'), ")$")
        ), '"')),
    # close where; group by needed for group_concat
    " ) GROUP BY _id;"
  )

  # get and process data, return
  return(dbiGetProcessData(
    src = src, key = key, statement = statement, n = n,
    fields = fields, subFields = subFields, rootFields = rootFields,
    params = params))

}


#' @export
docdb_query.src_postgres <- function(src, key, query, ...) {

  # make dotted parameters accessible
  params <- list(...)

  ## special case: return all fields if listfields != NULL
  if (!is.null(params$listfields)) {

    # get keys in top-level JSON object
    fields <- DBI::dbGetQuery(
      conn = src$con,
      statement = paste0(
        "SELECT DISTINCT jsonb_object_keys(json)",
        " FROM \"", key, "\";"
      ))[, 1, drop = TRUE]

    # return field names
    return(sort(fields))
  }

  ## add limit if not in params
  n <- -1L
  if (!is.null(params$limit)) n <- params$limit

  ## handle fields
  # all following is to emulate mongodb behaviour, which includes avoiding
  # path collisions with overlapping fields; see https://docs.mongodb.com/
  # manual/release-notes/4.4-compatibility/#path-collision-restrictions
  fields <- "{}"
  if (!is.null(params$fields)) fields <- params$fields
  fields <- json2fieldsSql(fields)

  ## special case: early return if only _id is requested
  if (query == "{}" && length(fields) == 1L && fields == "_id") {
    statement <- paste0(
      " SELECT DISTINCT ON (_id) _id FROM \"", key, "\" GROUP BY _id")
    out <- DBI::dbGetQuery(
      conn = src$con,
      n = n,
      statement = statement)
    return(out)
  }

  ## mangle fields for sql and jq
  # - requested root fields
  rootFields <- fields[!grepl("[.]", fields)]
  # - requested subfields
  subFields <- strsplit(fields[grepl("[.]", fields)], split = "[.]")
  #   keep only subFields that are not in rootFields
  subFields <- subFields[is.na(match(sapply(subFields, "[[", 1), rootFields))]

  ## convert from json to sql
  querySql <- json2querySql(query)

  ## construct sql query statement
  # head of sql
  # -1- no fields specified, get full documents
  if (length(fields) == 1L && fields == "") {
    statement <- paste0(
      "SELECT '{\"_id\": \"' || _id || '\", ' || LTRIM(json::TEXT, '{')
       AS json FROM \"", key, "\" ")
  }
  # -2- only _id is specified
  if (length(fields) == 1L && fields == "_id") {
    statement <- paste0(
      "SELECT '{\"_id\": \"' || _id || '\"}' AS json FROM \"", key, "\" ")
  }
  # -3- other fields specified
  if (length(fields[fields != "_id" & fields != ""]) >= 1L) {
    nonidFs <- c(rootFields[rootFields != "_id" & rootFields != ""],
                 sapply(subFields, "[[", 1))
    statement <- paste0(
      "SELECT json_build_object('_id', _id, ",
      paste0("'", nonidFs, "', json->'", nonidFs, "'", collapse = ", "),
      ") AS json FROM \"", key, "\" ")
  }

  # sql where for identifying documents and
  # for identifying jsonb_path_exists() rows
  if (any(querySql != "")) {

    statement <- paste0(
      statement, "WHERE ",
      paste0(
        sapply(querySql, function(x) paste0(
          ifelse(
            test = x[1] == "\"_id\"",
            # handle _id in query, since it is not in json
            yes = ifelse(
              # special handling of regular expression
              test = grepl("REGEXP", x[2]),
              yes = paste0(" _id ~ '", sub(".+\"(.+)\"", "\\1", x[2]), "'"),
              no = paste0(" _id ", gsub("\"", "'", x[2]), " ")
            ), # ifelse
            # querySql for other than _id
            no = paste0(
              " jsonb_path_exists(json, '$[*] ? (",
              ifelse(
                test = grepl("^ IN ", x[2]),
                yes = paste0(
                  "@.", x[1], " == ",
                  # split on comma after number or double quote, avoid splitting on comma in string
                  strsplit(gsub("([0-9\"]),", "\\1@", sub(" IN [(](.+)[)]", "\\1", x[2])), "@")[[1]],
                  collapse = " || "),
                no = paste0(
                  "@.",
                  ifelse(
                    test = grepl("'", x[1]),
                    yes = gsub("'", "", x[1]),
                    no = gsub("[.]", "\".\"", x[1])
                  ),
                  # special handling of regular expression
                  ifelse(
                    test = grepl("REGEXP", x[2]),
                    yes = sub("REGEXP ", " like_regex ", x[2]),
                    # special case equality
                    no = sub("^ = ", " == ", x[2])
                  ) # ifelse
                ) # paste0
              ), # ifelse
              ")') "
            ) # no
          ) # ifelse
        ), # paste0
        USE.NAMES = FALSE), # sapply
        # attr contains the logical operator
        collapse = attr(x = querySql, which = "op")
      ) # paste0
    ) # paste0
  } # if query

  # close statement
  statement <- paste0(statement, ";")

  # get and process data, return
  return(dbiGetProcessData(
    src = src, key = key, statement = statement, n = n,
    fields = fields, subFields = subFields, rootFields = rootFields,
    params = params))

}

#' @export
docdb_query.src_duckdb <- function(src, key, query, ...) {

  # make dotted parameters accessible
  params <- list(...)

  ## special case: return all fields if listfields != NULL
  if (!is.null(params$listfields)) {

    # get all fullkeys and types
    fields <- DBI::dbGetQuery(
      conn = src$con,
      statement = paste0(
        "SELECT DISTINCT json_group_structure(json)",
        " FROM \"", key, "\";"
      ))[, 1, drop = TRUE]

    # mangle from json
    fields <- sort(unique(names(unlist(jsonlite::fromJSON(fields)))))

    # return field names
    return(fields)
  }

  # ## add limit if not in params
  n <- -1L
  if (!is.null(params$limit)) n <- params$limit

  ## handle fields
  # the following is to emulate mongodb behaviour, which includes avoiding
  # path collisions with overlapping fields; see https://docs.mongodb.com/
  # manual/release-notes/4.4-compatibility/#path-collision-restrictions
  fields <- "{}"
  if (!is.null(params$fields)) fields <- params$fields
  fields <- json2fieldsSql(fields)

  ## special case: early return if only _id is requested
  if (query == "{}" && length(fields) == 1L && fields == "_id") {
    statement <- paste0(
      " SELECT DISTINCT ON (_id) _id FROM \"", key, "\" GROUP BY _id")
    out <- DBI::dbGetQuery(
      conn = src$con,
      n = n,
      statement = statement)
    return(out)
  }

  ## mangle fields for sql and jq
  # - requested root fields
  rootFields <- fields[!grepl("[.]", fields)]
  # - requested subfields
  subFields <- strsplit(fields[grepl("[.]", fields)], split = "[.]")
  #   keep only subFields that are not in rootFields
  subFields <- subFields[is.na(match(sapply(subFields, "[[", 1), rootFields))]

  ## convert from json to sql
  querySql <- json2querySql(query)

  # add query from subfields to include subfields
  rootFields <- unique(c(rootFields, unlist(sapply(
    querySql[grepl("[.]", sapply(querySql, "[[", 1))],
    function(i) sub("^\"?([^']+?)[.].+$", "\\1", i[1])))
  ))

  # https://duckdb.org/docs/extensions/json#json-extraction-functions

  ## construct sql and jq statements

  # dispatch querSql list elements depending on query:
  # - $and: any dot terms go into jqrSubsetFunction, rest in sqlQueryStatement
  # - $or and any element with dot: all go into jqrSubsetFunction
  tmp <- lapply(querySql, function(i) {

    if ((attr(querySql, "op") == "OR") ||
        (grepl("^[^'].+[.].+[^']$", gsub("[\"]", "", i[1])))) {
      c(i, "json")
    } else if (length(i) == 2L) {
      i[1] <- gsub("[']", "", i[1])
      c(i, "sql")
    } else {
      ""
    }
  })
  attr(tmp, "op") <- attr(querySql, "op")
  querySql <- tmp

  # identify fields other than _id for jq statement
  if (max(sapply(querySql, length)) == 3) {
    nonidFs <- sapply(querySql, "[[", 1)[sapply(querySql, "[[", 3) == "json"]
    nonidFs <- gsub('"', '', nonidFs)
  } else {
    nonidFs <- NULL
  }

  # sql SELECT
  # -1- no fields specified, get full documents
  if (length(fields) == 1L && fields == "") {
    statement <- paste0(
      "SELECT '{\"_id\": \"' || _id || '\", ' || LTRIM(json, '{')
       AS json FROM \"", key, "\" ")
  }
  # -2- only _id is specified
  if (length(fields) == 1L && fields == "_id") {
    # include other fields as relevant for any jq statement
    if (!length(nonidFs)) {
      statement <- paste0("SELECT '{\"_id\": \"' || _id || '\"}' AS json FROM \"", key, "\" ")
    } else {
      # include other fields as requested or relevant for any jq statement
      nonidFs <- unique(unlist(c(
        nonidFs,
        rootFields[rootFields != "_id" & rootFields != ""],
        sapply(subFields, "[[", 1)
      )))
      statement <- paste0(
        "SELECT json_object('_id', _id, ",
        paste0("'", nonidFs, "', json_extract(json, '$.", nonidFs, "')", collapse = ", "),
        ") AS json FROM \"", key, "\" ")
    }
  }
  # -3- other fields specified
  if (length(fields[fields != "_id" & fields != ""]) >= 1L) {
    # include other fields as requested or relevant for any jq statement
    nonidFs <- unique(unlist(c(
      nonidFs,
      rootFields[rootFields != "_id" & rootFields != ""],
      sapply(subFields, "[[", 1)
    )))
    statement <- paste0(
      "SELECT json_object('_id', _id, ",
      paste0("'", nonidFs, "', json_extract(json, '$.", nonidFs, "')", collapse = ", "),
      ") AS json FROM \"", key, "\" ")
  }

  # sql WHERE for identifying documents
  if (querySql[[1]][1] != "") {
    sqlQueryStatement <- unlist(lapply(
      querySql[sapply(querySql, "[[", 3) == "sql"],
      function(x) {
        if (grepl("^ IN ", x[2])) x[2] <- paste0(
          "REGEXP '", gsub("\"", "", paste0(
            # split on comma after number or double quote, avoid splitting on comma in string
            strsplit(gsub("([0-9\"]),", "\\1@", sub(" IN [(](.+)[)]", "\\1", x[2])), "@")[[1]],
            collapse = "|")), "'")
        if (x[1] != "\"_id\"") { # json
          ifelse(
            test = grepl("^REGEXP ", x[2]),
            yes = gsub("''+", "'", paste0(
              "CASE WHEN json_type(json, '$.", x[1], "') = 'VARCHAR'",
              " THEN regexp_matches(json_extract_string(json, '$.", x[1],
              "'), '", sub("^REGEXP \"?(.+?)\"?$", "\\1", x[2]), "')",
              " WHEN json_type(json, '$.", x[1], "') = 'ARRAY'",
              # since it is not possible to compare against values in an array,
              # compare to extracted string but modify regexp to use string delimiters
              " THEN regexp_matches(json_extract_string(json, '$.", x[1], "'), ",
              sub("'\\^", "'(^|[\",\\\\[])",
                  sub("\\$'", "([\",\\\\]]|$)'",
                      sub("(REGEXP| =) ", "", gsub('"(.+)"', "'\\1'", x[2]))
                  )), ") ELSE regexp_matches(json_extract(json, '$.", x[1], "'), '",
              sub("^REGEXP \"?(.+?)\"?$", "\\1", x[2]), "') END")),
            no = paste0(
              # duckdb 0.7.0 adding coalesce to capture json_type
              # returning NA or NULL if the path cannot be found
              "CASE WHEN coalesce(json_type(json, '$.", x[1], "'), '') ",
              "     IN ('VARCHAR', 'DOUBLE', 'BIGINT', 'UBIGINT', '') ",
              " THEN json_extract_string(json, '$.", x[1], "') ", gsub('"', "'", x[2]),
              " ELSE json_extract(json, '$.", x[1], "') ", gsub('"', "'", x[2]),
              " END")
          ) # ifelse
        } else { # _id
          ifelse(
            test = grepl("^REGEXP ", x[2]),
            yes = gsub("''+", "'", paste0(
              "regexp_matches(_id, '",
              sub("^REGEXP \"?(.+?)\"?$", "\\1", x[2]), "') ")),
            no = paste0(" _id ", gsub('"', "'", x[2]), " ")
          ) # ifelse
        } # _id or other field
      }))} # lapply unlist if
  #
  if (exists("sqlQueryStatement") && sum(nchar(sqlQueryStatement))) {
    statement <- paste0(
      statement, "WHERE ",
      paste0("( ", sqlQueryStatement, ")",
             collapse = paste0(" ", attr(querySql, "op"), " ")))
  }
  statement <- paste0(statement, "; ")

  # handle query terms with json
  jqrSubsetFunction <- NULL
  if (querySql[[1]][1] != "" &&
      any(sapply(querySql, "[[", 3) == "json")) {
    jqrSubsetFunction <- paste0(
      'def flatted: [paths(scalars) as $path | { ($path | map(tostring) | ',
      'join("#")): getpath($path) } ] | add; . | select( ',

      paste0("(", lapply(
        querySql[sapply(querySql, "[[", 3) == "json"],
        function(x) paste0(
          ' flatted | with_entries( select( .key | match( "',
          gsub("[.]", "#[0-9]+#", gsub("\"", "", x[1])),
          '" ))) | map(select(',
          ifelse(
            test = grepl("REGEXP", x[2]),
            yes = paste0(". | test(", gsub("(REGEXP| =) ", "", x[2]), ")))"),
            no = paste0(". ", gsub(" = ", " == ", x[2]), "))")
          ),
          " | any )")),
        collapse = paste0(" ", tolower(attr(querySql, "op")), " ")),
      " )")
  }

  # get and process data, return
  return(dbiGetProcessData(
    src = src, key = key, statement = statement, n = n,
    fields = fields, subFields = subFields, rootFields = rootFields,
    params = params, jqrSubsetFunction))

}

## helpers --------------------------------------

#' @keywords internal
#' @noRd
dbiGetProcessData <- function(
  statement, src, key, n, fields, subFields, rootFields, params,
  jqrSubsetFunction = NULL) {

  # minify statement
  statement <- gsub("[\n\t ]+", " ", statement)
  # adapt user regexp's such as \\S
  statement <- gsub("\\\\", "\\", statement, fixed = TRUE)

  # debug
  # message("\nSQL:\n\n", statement, "\n")
  # message("\nJQ:\n\n", jqrSubsetFunction, "\n")

  # temporary file and connection
  tfname <- tempfile()
  tfnameCon <- file(tfname, open = "wt", encoding = "native.enc")
  # register to close and remove file after used for streaming
  on.exit(try(close(tfnameCon), silent = TRUE), add = TRUE)
  on.exit(unlink(tfname), add = TRUE)

  ## get data, write to file in ndjson format ("\n")
  writeLines(
    paste0(
      "", # protect against empty query result
      # eliminate rows without any json
      stats::na.omit(
        DBI::dbGetQuery(
          conn = src$con,
          statement = statement,
          n = n)[["json"]])),
    con = tfnameCon,
    sep = "\n",
    useBytes = TRUE)
  close(tfnameCon)

  # early exit
  if (file.size(tfname) <= 1L) return(NULL)

  # to implement WHERE where needed, run jq line-by-line
  if (!is.null(jqrSubsetFunction)) {

    # compose jq string, target:
    #
    # def flatted: [paths(scalars) as $path | { ($path | map(tostring) |
    # join("#")): getpath($path) } ] | add; . | select(
    # ( flatted | with_entries( select( .key |
    #   match( "friends#[0-9]+#id" )))
    #   | map(select( . < 2)) | any )
    # and
    # ( flatted | with_entries( select( .key | match( "friends#[0-9]+#name" )))
    #   | map(select( . == "Lacy Chen")) | any )
    # )

    # get another file name
    tjname <- tempfile()
    # register to remove file after used for streaming
    on.exit(unlink(tjname), add = TRUE)

    # write data
    jqr::jq(
      file(tfname),
      jqrSubsetFunction,
      flags = jqr::jq_flags(pretty = FALSE),
      out = tjname
    )

    # early exit
    if (!file.size(tjname)) return(NULL)

    # swap file name
    tfname <- tjname

  } # if subFields

  # to extract subFields if any, run jq line-by-line
  if (length(subFields)) {

    # inform user (may not be precise)
    if (any(duplicated(unlist(subFields)))) {
      message("Note: paths seem to (partially) overlap as specified in ",
              "'fields', last will be used: ", paste0(fields, collapse = ", "))}

    # compose jq script, target:
    # {"_id", "friends": {"id": [."friends" | (if type != "array" then [.] else .[] end) | ."id"] }}
    # "{\"_id\", \"friends\": {\"id\":  [.\"friends\" | (if type != \"array\" then [.][] else .[] end) | .\"id\"]}}"
    jqFields <- ifelse(!length(rootFields), "", paste0('"', rootFields, '"', collapse = ", "))
    if (jqFields != "") jqFields <- paste0(jqFields, ", ", collapse = "")
    jqFields <- paste0('{', paste0(c(jqFields, paste0(
      sapply(
        subFields,
        function(s) {
          # first item
          k <- paste0('"', s[1], '": ')
          v <- paste0(' [."', s[1], '" | (if type != "array" then [.][] else .[] end) | ')
          # next item
          for (i in (seq_len(length(s) - 2) + 1)) {
            k <- paste0(k, '{"', s[i], '": ')
            v <- paste0(v, '."', s[i], '" | (if type != "array" then [.][] else .[] end) | ')
          }
          # last item
          k <- paste0(k, '{"', s[length(s)], '": ')
          v <- paste0(v, '."', s[length(s)], '"]')
          # combine item
          paste0(k, v, paste0(rep("}", length(s) - 1), collapse = ""))
        }, USE.NAMES = FALSE),
      collapse = ", ")), collapse = ""), "}")

    # get another file name
    tjname <- tempfile()
    # register to remove file after used for streaming
    on.exit(unlink(tjname), add = TRUE)

    # write data
    jqr::jq(
      file(tfname),
      jqFields,
      flags = jqr::jq_flags(pretty = FALSE),
      out = tjname
    )

    # early exit
    if (!file.size(tjname)) return(NULL)

    # swap file name
    tfname <- tjname

  } # if subFields

  # stream_in automatically opens and later closes (and destroys) connection
  out <- jsonlite::stream_in(file(tfname), verbose = FALSE)

  # exclude any root fields with name:0 or that were not specified
  if (length(params[["fields"]])) {
    # any fields that were to be included
    rM <- NULL
    m <- stringi::stri_match_all_regex(params[["fields"]], '"([-@._\\w]+?)":[ ]*1')[[1]][, 2, drop = TRUE]
    if (!is.na(m[1])) rM <- stats::na.omit(match(findColumnNames(m, out), names(out)))
    if (length(rM)) out <- out[, rM, drop = FALSE]
    # any fields that were requested to not be included
    rM <- NULL
    m <- stringi::stri_match_all_regex(params[["fields"]], '"([-@._\\w]+?)":[ ]*0')[[1]][, 2, drop = TRUE]
    if (!is.na(m[1])) rM <- m
    if (!is.na(m[1]) && length(rM)) rM <- stats::na.omit(match(rM, names(out)))
    if (length(rM)) out <- out[, -rM, drop = FALSE]
    #
  }

  # to emulate mongo behaviour eliminate *columns* of all nulls
  # note jsonlite::stream_in turns null values into NAs
  out <- out[, seq_len(ncol(out))[!sapply(out, function(r) all(is.na(r)))], drop = FALSE]

  # early return
  if (identical(names(out), "_id") || nrow(out) <= 1L) return(out)

  # remove *rows* with all NAs except in _id
  naout <- is.na(out)
  nc <- length(setdiff(attr(naout, "dimnames")[[2]], "_id"))

  # return
  return(out[rowSums(naout) < nc, , drop = FALSE])

}


# converts json string into
# string with sql fields
#' @keywords internal
#' @noRd
json2fieldsSql <- function(x) {

  # validate
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

  # simplify
  x <- unique(x)
  x <- x[x != ""]
  if (!length(x)) x <- ""

  # return
  return(x)

}
# json2fieldsSql(x = '{"cut" : "1", "price": 1 }') # "cut" not returned because 1 is string
# json2fieldsSql(x = '{"_id": 1, "annotation": 0, "other": 1}') # correctly specified numbers
# json2fieldsSql('{}')


# convert json query string to
# string as part for sql WHERE
#' @keywords internal
#' @noRd
json2querySql <- function(x) {

  # validate
  if (!jsonlite::validate(x)) stop("No json: ", x)

  # standard operation
  op <- "AND"

  # empty query
  if (x == "{}") {
    out <- ""
    attr(out, "op") <- op
    return(out)
  }

  # stop if more than one operator
  if (stringi::stri_count_regex(x, pattern = '[$]and|[$]or":') >= 2L) {
   stop("Can use at most one operator ($or, $and), but two or more in 'query'",
        call. = FALSE)
  }

  # minify for regexprs
  x <- jsonlite::minify(x)

  # remove any options part as used in mongodb
  x <- sub(pattern = ',? *?"[$]options" *: *".*?"', replacement = " ", x = x)

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

  # find which commas can be used for splitting
  x <- gsub(",([\"\\w{}.]+:)", "~\\1", x, perl = TRUE)
  x <- unlist(strsplit(x = x, split = "~", fixed = TRUE))

  # get left hand side and remove special characters
  LHS <- sapply(strsplit(x = x, split = ":"), "[[", 1)
  LHS <- trimws(gsub("[{} ]", "", LHS))

  # get right hand side, that is, operator and value
  x <- gsub(pattern = ".*?:(.*)", replacement = "\\1", x = x)

  # remove brackets, blanks and quotation marks around operator
  x <- gsub(pattern = "[{]([,0-9]+)[}]", replacement = "|##\\1##|", x = x)
  x <- gsub(pattern = "[{}]", replacement = "", x = x)
  x <- gsub(pattern = "[|]##", replacement = "{", x = x)
  x <- gsub(pattern = "##[|]", replacement = "}", x = x)
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
  x <- gsub(pattern = "[$]in:\\[?([^]]+)\\]?", replacement = " IN (\\1)", x = x, perl = TRUE)

  # special case
  # https://docs.mongodb.com/manual/reference/operator/query/regex/#pcre-vs-javascript
  x <- gsub(pattern = "[$]regex:", replacement = "REGEXP ", x = x)
  # make right hand side
  RHS <- x

  # concatenate for return
  out <- lapply(seq_along(LHS), function(x) c(LHS[x], RHS[x]))

  # add logical operation
  attr(out, "op") <- op

  # debug
  # print(out)

  # return
  return(out)

}
# json2querySql(x = '{"cut" : "Premium", "price" : { "$lt" : 1000 }, "_id": {"$ne": 4}, "_row": {"$regex": "Merc"} }')
# json2querySql(x = '{ "$or": [ { "gear": { "$lt": 5 } }, { "cyl": 6 } ] }')
# json2querySql(x = '{ "$or": [ { "gear": { "$in": [4,5] } }, { "cyl": 6 } ] }')
# json2querySql(x = '{ "$or": [ { "name": { "$in": ["Lucy Chen", "Amy"] } }, { "cyl": 6 } ] }')
# json2querySql(x = '{ "name": { "$in": ["Lucy Chen", "Amy"] }, "cyl": 6 }')
# json2querySql(x = '{"mpg": {"$lte": 18}, "gear": {"$gt": 3}}')
# json2querySql(x = '{"mpg": {"$lte": 18}, "gear": 4}')
# json2querySql(x = '{"mpg": {"$lte": 18}, {"gear": 4}}') # no json
# json2querySql(x = '{"mpg": [{"$lte": 18}, {"gear": 4}]}')
# json2querySql(x = '{"$or": [{"age": {"$gte": 23}}, {"friends.name": "Dona Bartlett"}]}')
# json2querySql(x = "{}")
# json2querySql(x = '{"_id": { "$regex": "^NCT[0-9]{7,8}"} }')
# json2querySql(x = '{"gear": {"$in": [5,4]}}')
# json2querySql(x = '{ "$or": [{ "$or": [ { "gear": { "$lt": 5 } }, { "cyl": 6 } ] }, {"gear": {"$in": [5,4]}}] }') # error

# generate regular expressions for
# fields so that fullkey matches
#' @keywords internal
#' @noRd
fieldsSql2fullKey <- function(x) {

  # check e.g. if only fields = '{"_id": 1}'
  if (!length(x)) return("")

  # remove single quotes used for preventing dot interpretation
  x <- gsub("[']", "", x)

  # remove array indices from fields
  x <- gsub("\\[[-#0-9]+\\][.]", ".", x)

  # protect "." between item and subitem using lookahead for overlapping groups
  x <- gsub("([a-zA-Z]+)[.](?=[a-zA-Z]+)", "\\1@@@\\2", x, perl = TRUE)

  # add in regexps to match any arrayIndex in fullkey
  x <- paste0("[$][.]", gsub("@@@", "[-#\\\\[\\\\]0-9]*[.]", x), "")

  # return
  return(x)
}


# map fields and subfields to columns of out
#' @keywords internal
#' @noRd
findColumnNames <- function(m, out) {

  assert(out, "data.frame")

  namesSubItems <- lapply(
    seq_len(ncol(out)),
    function(s) {
      tmp <- jqr::jq(
        jsonlite::toJSON(out[1, s, drop = FALSE]),
        ' [ paths | map(select(type != "number")) | select(length > 0) | join(".") ] | unique | .[] ')
      class(tmp) <- "character"
      tmp <- gsub('"', "", tmp)
      unique(c(names(out)[s], tmp))
    }
  )

  namesOut <- sapply(seq_along(namesSubItems),
                     function(i) if (any(m %in% namesSubItems[[i]])) i)
  namesOut <- unlist(namesOut)

  return(names(out)[namesOut])

}
