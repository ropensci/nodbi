#' Get documents or parts with filtering query
#'
#' Uses [jqr] for complementing the databases'
#' native query and filtering functions.
#' If \code{query = "{}"} and neither `fields`
#' nor `listfields` is specified, runs [docdb_get()].
#'
#' @inheritParams docdb_create
#'
#' @param query (character) A JSON query string, see examples.
#'  Can use comparisons / tests (`$lt`, `$lte`, `$gt`, `$gte`,
#'  `$ne`, `$in`, `$regex`), with logic operators (`$and`,
#'  `$or`, `(`, `)`), including nested queries, see examples.
#'
#' @param ... Optionally, specify `fields` as a JSON string of
#' fields to be returned from anywhere in the tree, see examples.
#' Alternatively, specify `listfields` with any value to obtain a
#' vector of all fields in dot path notation.
#'
#' @note A dot in `query` or `fields` is interpreted as a dot path,
#' pointing to a fields nested within another, e.g. `friends.id` in
#' example below.
#'
#' @return Data frame with requested documents, may have nested
#'  lists in columns.
#'
#' @export
#'
#' @examples \dontrun{
#' src <- src_sqlite()
#'
#' docdb_create(src, "myKey", mtcars)
#' docdb_create(src, "myKey", contacts)
#' docdb_create(src, "myKey", mapdata)
#'
#' docdb_query(src, "myKey", query = '{"mpg":21}')
#' docdb_query(src, "myKey", query = '{"mpg":21, "gear": {"$lte": 4}}')
#' docdb_query(src, "myKey", query = '{"mpg":21}', fields = '{"_id":0, "mpg":1, "cyl":1}')
#' docdb_query(src, "myKey", query = '{"_id": {"$regex": "^.+0.*$"}}', fields = '{"gear": 1}')
#'
#' # complex query, not supported for src_elastic and src_couchdb backends at this time:
#' docdb_query(src, "myKey", query = '{"$and": [{"mpg": {"$lte": 18}}, {"gear": {"$gt": 3}}]}')
#' docdb_query(src, "myKey", query = '{}', fields = '{"_id":0, "mpg":1, "cyl":1}')
#'
#' docdb_query(src, "myKey", query = '{"$and": [{"age": {"$gt": 21}},
#'  {"friends.name": {"$regex": "^B[a-z]{3,9}.*"}}]}')
#' docdb_query(src, "myKey", query = '{"$or": [{"rows.elements.status": "OK"}, {"$and": [
#'  {"_id": "5cd6785325ce3a94dfc54096"}, {"friends.name": {"$regex": "^B[a-z]{3,90}.*"}}]}]}')
#' docdb_query(src, "myKey", query = '{"$and": [{"_id": "5cd6785325ce3a94dfc54096"},
#'  {"friends.name": {"$regex": "^B[a-z]{3,90}.*"}}]}')
#' docdb_query(src, "myKey", query = '{"origin_addresses": {"$in": ["Santa Barbara, CA, USA",
#'  "New York, NY, USA"]}}', fields = '{"age": 1, "friends.id": 1, "_id": 0,
#'  "rows.elements.status": 1}')
#'
#' docdb_query(src, "myKey", query = '{"rows.elements.status": "OK"}', listfields = TRUE)
#'
#' }
docdb_query <- function(src, key, query, ...) {

  assert(src, "docdb_src")
  assert(key, "character")
  assert(query, c("json", "character"))

  if (query == "") {
    warning('query = "" is deprecated, use query = "{}"')
    query <- "{}"
  }
  stopifnot(jsonlite::validate(query))

  # query can be empty but then fields should not be empty
  if (jsonlite::minify(query) == '{}') {

    # mangle parameters
    params <- list(...)
    if (is.null(params$listfields) &&
        (is.null(params$fields) ||
         jsonlite::minify(params$fields) == '{}')) {
      message("Parameters query, fields and listfields empty, calling docdb_get()")
      params$query <- NULL
      params$fields <- NULL
      params$listfields <- NULL
      #if (!length(params)) params <- NULL
      if (is.list(params) &&
          !is.null(params$limit)) {
        limit <- params$limit
        params$limit <- NULL
      } else {
        limit <- NULL
      }
      if (!length(names(params))) params <- NULL

      # dispatch
      return(do.call(docdb_get, list(src = src, key = key, limit = limit, params)))

    }}

  # dispatch
  UseMethod("docdb_query", src)

}



#' @export
docdb_query.src_couchdb <- function(src, key, query, ...) {

  # https://cran.r-project.org/web/packages/sofa/vignettes/query_tutorial.html

  # make dotted parameters accessible
  limit <- 9999999L
  params <- list(...)
  if (!is.null(params[["limit"]])) {
    limit <- params$limit
    params$limit <- NULL
  }

  # check parameters
  fields <- '{}'
  if (!is.null(params[["fields"]])) fields <- params[["fields"]]
  stopifnot(jsonlite::validate(fields))
  query <- jsonlite::minify(query)

  # digest and mangle
  fldQ <- digestFields(f = fields, q = query)


  # mangle query

  # - where
  fldQ$jqrWhere <- character(0L)
  if (length(fldQ$queryCondition) &&
      (!identical(fldQ$queryFields, fldQ$queryRootFields) ||
       grepl("\"\\$[orinadegxlt]+\":", query))) {

    fldQ$jqrWhere <- fldQ$queryJq

    query <- "{}"

  }

  # - handle search on top level where
  #   a field could be an array, see
  #   https://docs.couchdb.org/en/stable/api/database/find.html#find-elemmatch
  if (query != "{}" &&
      length(fldQ$queryFields) &&
      identical(fldQ$queryFields, fldQ$queryRootFields)) {

    # add missing $eq (missing $and does not seem needed)
    query <- gsub("(\"[^\\$]+\":) *([^{]+)([,}$])", "\\1{\"$eq\":\\2}\\3", query)

    # use jq to extract
    for (i in fldQ$queryFields) {

      subQuery <- jqr::jq(query, paste0(" .. | .\"", i, "\"? | select (. != null) "))
      subQuery <- as.character(subQuery)
      subQueryIns <- gsub("^\\{|\\}$", "", subQuery)
      subQueryMod <- paste0('{"$or":[{"$elemMatch":{', subQueryIns, '}},{', subQueryIns, '}]}')

      # integrate
      query <- stringi::stri_replace_all_fixed(
        query, subQuery, subQueryMod, vectorize_all = F
      )

    }

  }

  # - remove any regexp options
  query <- gsub(',? *?"[$]options" *: *".*?"', "", query)


  # fields

  # - add fields
  if (length(fldQ$includeRootFields)) {

    if (length(fldQ$includeRootFields) &&
        !any("_id" == fldQ$includeRootFields)) fldQ$includeRootFields <- c("_id", fldQ$includeRootFields)

    tmpFields <- fldQ$includeRootFields
    if (length(fldQ$jqrWhere)) tmpFields <- unique(c(tmpFields, fldQ$queryRootFields))

    addQuery <- paste0('{"', tmpFields, '":{"$exists":true}', "}", collapse = ",")
    if (query == "{}" && length(tmpFields)) query <- paste0('{"$and":[', addQuery, ']}')

    addQuery <- paste0('"fields": [', paste0(
      '"', tmpFields, '"', collapse = ", "), ']')
    if (length(tmpFields)) query <- paste0(query, ", ", addQuery)

  }

  # - add selector
  query <- paste0('"selector": ', query)

  # - add limit and close query
  query <- paste0('{', query, ', "limit": ', limit, '}')


  # special case: return all fields if listfields != NULL
  if (!is.null(params$listfields)) {

    j <- 'path(..) | join (".") '
    fldQ$jqrWhere <- ifelse(
      length(fldQ$jqrWhere), paste0(fldQ$jqrWhere, " | ", j), j)

    fields <- processDbGetQuery(
      getData = 'jqr::jq(do.call(sofa::db_query, c(list(cushion = src$con,
                 dbname = key, query = query, as = "json"), params)),
                 ".docs[] | del(._rev)" )',
      jqrWhere = fldQ$jqrWhere)[["out"]]

    # mangle "friends.0", "friends.0.id"
    fields <- unique(gsub("[.][0-9]+", "", fields))
    fields <- fields[fields != "_id"]
    fields <- fields[fields != ""]

    # return field names
    return(sort(fields))

  }


  # debug
  if (options()[["verbose"]]) {
    message("\nquery: ", query, "\n")
  }


  # general processing
  return(processDbGetQuery(
    getData = 'jqr::jq(do.call(sofa::db_query, c(list(cushion = src$con,
               dbname = key, query = query, as = "json"), params)),
               ".docs[] | del(._rev)" )',
    jqrWhere = fldQ$jqrWhere,
    outputFields = fldQ$includeFields,
    excludeFields = fldQ$excludeFields))

}



#' @export
docdb_query.src_elastic <- function(src, key, query, ...) {

  # https://www.elastic.co/guide/en/elasticsearch/reference/current/query-dsl.html

  # make dotted parameters accessible
  limit <- 10000L # see help(Search)
  params <- list(...)
  if (!is.null(params[["limit"]])) {
    limit <- params$limit
    params$limit <- NULL
  }

  # check parameters
  fields <- '{}'
  if (!is.null(params[["fields"]])) fields <- params[["fields"]]
  stopifnot(jsonlite::validate(fields))
  query <- jsonlite::minify(query)


  # digest and mangle
  fldQ <- digestFields(f = fields, q = query)


  # query

  # - where for any query that is not just simply
  #   for the identify of one ore more root fields
  fldQ$jqrWhere <- character(0L)
  if (length(fldQ$queryCondition) &&
      (!identical(fldQ$queryFields, fldQ$queryRootFields) ||
       grepl("\"\\$[orinadegxlt]+\":", query))) {

    fldQ$jqrWhere <- fldQ$queryJq

    query <- "*"

  }

  # https://www.elastic.co/guide/en/elasticsearch/reference/current/query-dsl-query-string-query.html#query-string-syntax
  # where the status field contains active: 'status:active'
  # where the field title has any non-null value: '_exists_:title'

  # fields

  # - add fields
  if (length(fldQ$includeRootFields) ||
      length(fldQ$queryRootFields)) {

    addQuery <- paste0(
      '_exists_:', unique(c(fldQ$includeRootFields, fldQ$queryRootFields)), collapse = " OR ")

    if (length(fldQ$includeRootFields) ||
        length(fldQ$queryRootFields))
      query <- paste0(query, ' AND (', addQuery, ')')

  }

  # - if no includeFields, get all fields
  if (!length(fldQ$includeFields)) fldQ$queryFields <- character(0L)

  # - implement query syntax for Search_uri

  # q = '(_id:5cd67853f841025e65ce0ce2)'
  # q = '*'

  # - remove quotes around field name, mangle
  if (length(fldQ$queryCondition) &&
      grepl(" == ", fldQ$queryCondition) &&
      !grepl("[<>]| REGEXP | IN ", fldQ$queryCondition)) {
    addQuery <- fldQ$queryCondition
    addQuery <- gsub(" == ", ":", addQuery)
    addQuery <- gsub('"(.+?)":', "\\1:", addQuery)
    addQuery <- gsub("'", '"', addQuery)
    if (length(addQuery)) query <- sub("\\{.*\\}", addQuery, query)
    query <- sub("^\\* AND ", "", query)
  }

  # - integrate
  if (grepl("^\\{\\}", query)) query <- sub("\\{\\}", "*", query)
  if (query == "") query <- "*"


  # debug
  if (options()[["verbose"]]) {
    message("\nq: ", query, "\n")
  }


  # process

  # - obtain ids and process
  docids <- elastic::Search_uri(
    conn = src$con, index = key,
    source = FALSE, size = limit,
    q = query, verbose = FALSE)[["hits"]][["hits"]]

  # - process ids
  if (!length(docids)) return(NULL)
  docids <- docids[sapply(docids, "[[", "_score") >= 1.0]
  docids <- sort(sapply(docids, "[[", "_id"))
  if (is.null(docids)) return(NULL)

  # - debug
  if (options()[["verbose"]]) {
    message("\nlength docids: ", length(docids), "\n")
    message("\nsource_includes: ", paste0(fldQ$includeFields, collapse = " / "), "\n")
  }

  # - early exit
  if (length(fldQ$includeFields) &&
      !length(fldQ$jqrWhere) &&
      all(fldQ$includeFields == "_id")) return(data.frame(
        "_id" = docids, stringsAsFactors = FALSE, check.names = FALSE))

  # - function to get documents
  if (length(docids) == 1L) {
    getData <- 'jqr::jq(elastic::docs_get(conn = src$con, index = key, id = docids[1],
               verbose = FALSE, raw = TRUE, source_includes = c(fldQ$includeFields, fldQ$queryFields)),
               " ._source._id = ._id | ._source ")'
  } else {
    getData <- 'jqr::jq(textConnection(elastic::docs_mget(conn = src$con,
                index = key, ids = docids, verbose = FALSE,
                raw = TRUE, source = c(fldQ$includeFields, fldQ$queryFields))),
                " .docs[] | ._source._id = ._id | ._source ")'
  }

  # - special case: listfields
  if (!is.null(params$listfields)) {

    # https://www.elastic.co/guide/en/elasticsearch/reference/current/indices-get-mapping.html
    # could theoretically be used but no query seems possible to limit fields to found docs

    j <- 'path(..) | join (".") '
    fldQ$jqrWhere <- ifelse(
      length(fldQ$jqrWhere), paste0(fldQ$jqrWhere, " | ", j), j)

    fields <- processDbGetQuery(
      getData = sub(", source = fldQ\\$includeFields", "", getData),
      jqrWhere = fldQ$jqrWhere)[["out"]]

    # mangle "friends.0", "friends.0.id"
    fields <- unique(gsub("[.][0-9]+", "", fields))
    fields <- fields[fields != "_id"]
    fields <- fields[fields != ""]

    # return field names
    return(sort(fields))

  }

  # - generate results
  return(processDbGetQuery(
    getData = getData,
    jqrWhere = fldQ$jqrWhere,
    outputFields = fldQ$includeFields,
    excludeFields = fldQ$excludeFields))

}



#' @export
docdb_query.src_mongo <- function(src, key, query, ...) {


  # check name of collection
  chkSrcMongo(src, key)


  # query
  query <- jsonlite::minify(query)


  # make dotted parameters accessible
  params <- list(...)


  # canonical sorting in nodbi
  if (!length(params[["sort"]])) params[["sort"]] <- '{"_id": 1}'
  if (!length(params[["fields"]])) params[["fields"]] <- '{}'


  # if regexp query lacks options, add them in
  if (grepl('"[$]regex" *: *".+?" *}', query)) query <-
    sub('("[$]regex" *: *".+?" *)', '\\1, "$options": ""', query)


  # digest and mangle
  fldQ <- digestFields(f = params[["fields"]], q = query)


  # query

  # - make sure fields in query exist in records
  #   purpose: match with sqlite, postgres, duckdb
  if (length(fldQ$queryRootFields[fldQ$queryRootFields != "_id"]) ||
      length(fldQ$includeRootFields[fldQ$includeRootFields != "_id"])) {

    # $exists matches the documents that contain the field,
    # including documents where the field value is null
    tf <- unique(c(fldQ$queryRootFields, fldQ$includeRootFields))
    addQuery <- paste0("{", paste0(
      '"', tf[tf != "_id"],
      '":{"$exists":true, "$ne": []}'), "}", collapse = ",")
    # sufficient that any exists, however $or is costly in mongo
    addQuery <- paste0('{"$or": [', addQuery, ']}')
    # integrate
    if (query == '{}') query <- addQuery else
      query <- paste0('{"$and":[', sub("", "", query), ",", addQuery, ']}')

  }


  # fields

  # - handle fields to be excluded which would trigger the mongo error
  #   Cannot do exclusion on field ... in inclusion projection
  if (length(fldQ$excludeFields)) {
    if (length(fldQ$includeFields)) {
      params[["fields"]] <- paste0(
        '{', paste0('"', fldQ$includeFields, '": 1', collapse = ", "), "}")
    } else {
      params[["fields"]] <- '{}'
    }
  }

  # - handle Path collision with overlapping fields
  if (any(duplicated(unlist(
    strsplit(fldQ$includeFields, ".", fixed = TRUE))))) {
    params[["fields"]] <- paste0(
      '{', paste0('"', fldQ$includeRootFields, '": 1', collapse = ", "), "}")
  }


  # debug
  if (options()[["verbose"]]) {
    message("\nQuery: ", query, "\n")
    message("\nParams: ", params, "\n")
  }


  # early return if listfields
  if (!is.null(params$listfields)) {

    # try mapreduce to get all keys
    fields <- try({
      src$con$mapreduce(
        map = "function() {
      obj = this;
      return searchInObj(obj, '');
      function searchInObj(obj, pth){
         for(var key in obj){
            if(typeof obj[key] == 'object' && obj[key] !== null){
               if(pth != '') {pth = pth + '.'}
                  searchInObj(obj[key], pth + key);
            }else{
               key = pth + '.' + key;
               key = key.replace(/[.][0-9]+[.]/g, '.');
               key = key.replace(/[.][0-9]+$/, '');
               key = key.replace(/[.][.]+/g, '.');
               key = key.replace(/^[.]/, '');
               emit(key, 1);
      }}}}",
      reduce = "function(i) {return i}",
      query = query
      )[["_id"]]}, silent = TRUE)

    # alternative approach
    if (inherits(fields, "try-error")) {

      # get files for
      tf <- tempfile()
      on.exit(try(unlink(tfname), silent = TRUE), add = TRUE)

      # NOTE mass export
      src$con$export(con = file(tf), query = query)

      # use jq
      fields <- as.character(jqr::jq(file(tf), 'path(..) | join (".") '))

    } # if try-error

    # mangle "\"item.0.sub\"
    fields <- gsub('"|[.][0-9]+', "", fields)

    # add parent fields e.g. "friends" for "friends.id"
    parentFields <- strsplit(fields[grepl("[.]", fields)], ".", fixed = TRUE)
    parentFields <- lapply(parentFields, function(i) sapply(
      rev(seq_along(i))[-1], function(ii) paste0(i[1:ii], collapse = ".")))
    parentFields <- unique(unlist(parentFields))

    # format
    fields <- c(fields, parentFields)
    fields <- unique(fields)
    fields <- fields[fields != ""]
    fields <- fields[fields != "_id"]

    # return
    return(sort(fields))

  } # !is.null(params$listfields)


  # processing

  # - early return
  if (!any(grepl("[.]", fldQ$includeFields)) &&
      !length(fldQ$excludeFields)) return(
        do.call(
          src$con$find,
          c(list(query = query), params))
      )


  # - jq to extract fields and subfields
  tfname <- tempfile()
  on.exit(try(unlink(tfname), silent = TRUE), add = TRUE)


  # - fldQ$includeFields has subfields thus mangle
  if (length(fldQ$includeRootFields)) params[["fields"]] <- paste0(
    '{', paste0('"', fldQ$includeRootFields, '": 1', collapse = ", "), "}")


  # - process
  do.call(
    src$con$export,
    c(list(
      con = file(tfname),
      query = query),
      params)
  )

  # - early exit
  if (!length(fldQ$excludeFields)) return(
    processOutputFields(tfname, fldQ$includeFields))

  # - any include
  if (length(fldQ$includeFields)) {

    # get another file name
    tjname <- tempfile()
    on.exit(try(unlink(tjname), silent = TRUE), add = TRUE)

    # write data
    processOutputFields(tfname, fldQ$includeFields, tjname)

    # early exit
    if (!file.size(tjname)) return(NULL)

    # swap file name for next processing step
    tfname <- tjname

  }

  # - excludes
  return(
    processExcludeFields(
      jsonlite::stream_in(
        file(tfname),
        verbose = FALSE),
      fldQ$excludeFields)
  )

}



#' @export
docdb_query.src_sqlite <- function(src, key, query, ...) {


  # make dotted parameters accessible
  params <- list(...)


  # query
  query <- jsonlite::minify(query)


  # add limit if not in params
  n <- -1L
  if (!is.null(params$limit)) n <- params$limit


  # add fields if not in params
  fields <- "{}"
  if (!is.null(params$fields)) fields <- jsonlite::minify(params$fields)


  # digest
  fldQ <- digestFields(f = fields, q = query)


  # fields

  # - early return if only _id is requested
  if (query == "{}" &&
      length(fldQ$includeFields) == 1L &&
      fldQ$includeFields == "_id") {

    statement <- paste0(
      "SELECT DISTINCT _id ",
      "FROM \"", key, "\";")

    return(DBI::dbGetQuery(
      conn = src$con,
      n = n, statement = statement))

  }

  # - continue field mangling
  if (length(fldQ$includeFields)) {

    if (length(fldQ$includeFields) == 1L &&
        fldQ$includeFields == "_id") {

      fldQ$composeJson <- '\'{"_id":"\' || _id || \'"}\''
      fldQ$includeFields <- character(0L)

    } else {

      fldQ$composeJson <- '
    \'{"_id":"\' || _id || \'",\' || group_concat(\'"\' || fkr || \'":\' ||
    IIF(type = \'text\', CONCAT(\'"\', REPLACE(value, \'"\', \'\\"\'), \'"\'), value)
    ) || \'}\''

    }
  } else {

    fldQ$composeJson <- '
    \'{"_id":"\' || _id || \'",\' || LTRIM(value, \'{\')'

  }


  # query

  # - using root fields so as to later mangle json with jqr,
  #   since RSQLite does not include regex_replace() out of
  #   the box, but this would be needed to remove from column
  #   value the quantifiers between fields in fullkey or path
  if (length(fldQ$includeRootFields)) fldQ$includeRootFields <- paste0(
    'AND ( fkr REGEXP "^(', paste0(
      fldQ$includeRootFields, collapse = "|"),
    ')$" AND value <> "{}" ) ')

  # - map query to row-wise testing
  fldQ$queryCondition <- gsub("'", '"', fldQ$queryCondition)
  for (i in fldQ$queryPaths) {

    fldQ$queryCondition <- gsub(
      paste0("(\"", i, "\") ([INOTREGXP=!<>']+ .+?)( AND | NOT | OR |\\)*$)"),
      paste0('SUM ( fkr REGEXP "^',
             gsub('"[.]"', "[-#\\\\\\\\[\\\\\\\\]0-9]*[.]", i),
             '" AND value \\2 ) > 0 \\3'),
      fldQ$queryCondition
    )
  }

  # _id in query
  fldQ$queryCondition <- gsub(
    'fkr REGEXP \\"\\^_id\\" AND value',
    "_id", fldQ$queryCondition)

  # - empty query
  if (!length(fldQ$queryCondition)) fldQ$queryCondition <- "TRUE"

  # statement
  statement <- insObj('
    WITH extracted AS ( SELECT _id, value, type,
     LTRIM(REPLACE(fullkey, \'"\', \'\'), \'$.\') AS fkr
    FROM "/** key **/", json_tree("/** key **/".json)
    WHERE value <> "{}" )
    SELECT /** fldQ$composeJson **/
    AS json FROM extracted WHERE ( _id IN (
    SELECT _id FROM extracted GROUP BY _id HAVING (
    /** fldQ$queryCondition **/  ) )
    /** fldQ$includeRootFields **/
    ) GROUP BY _id;')


  # special case: return all fields if listfields != NULL
  if (!is.null(params$listfields)) {

    # fallback but only for listfields
    if (!length(fldQ$queryCondition)) fldQ$queryCondition <- "TRUE"

    # statement
    statement <- insObj('
      WITH extracted AS ( SELECT _id, value,
        LTRIM(REPLACE(fullkey, \'"\', \'\'), \'$.\') AS fkr
      FROM "/** key **/", json_tree("/** key **/".json)
      WHERE value <> "{}" )
      SELECT DISTINCT fkr
      AS flds FROM extracted WHERE ( _id IN (
      SELECT _id FROM extracted GROUP BY _id HAVING (
      /** fldQ$queryCondition **/  ) ) )
      ORDER BY flds;')

    # get all fullkeys and types
    fields <- DBI::dbGetQuery(
      conn = src$con,
      statement = statement,
      n = -1L)[, "flds", drop = TRUE]

    # remove "$.", array elements "$.item[0]"
    fields <- unique(gsub("\\$[.]?|\\[[-#0-9]+\\]", "", fields))
    fields <- fields[fields != ""]

    # return field names
    return(fields)
  }


  # regular processing
  return(processDbGetQuery(
    getData = 'paste0(DBI::dbGetQuery(conn = src$con,
               statement = statement, n = n)[["json"]], "")',
    outputFields = fldQ$includeFields,
    excludeFields = fldQ$excludeFields))

}


#' @export
docdb_query.src_postgres <- function(src, key, query, ...) {


  # make dotted parameters accessible
  params <- list(...)


  # add limit if not in params
  n <- -1L
  if (!is.null(params$limit)) n <- as.integer(params$limit)


  # add fields if not in params
  fields <- "{}"
  if (!is.null(params$fields)) fields <- jsonlite::minify(params$fields)


  # query
  query <- jsonlite::minify(query)


  # digest
  fldQ <- digestFields(f = fields, q = query)


  # - early return if only _id is requested
  if (query == "{}" &&
      length(fldQ$includeFields) == 1L &&
      fldQ$includeFields == "_id") {

    statement <- paste0(
      "SELECT DISTINCT ON (_id) _id ",
      "FROM \"", key, "\" GROUP BY _id;")

    return(DBI::dbGetQuery(
      conn = src$con,
      n = n, statement = statement))

  }


  # fields

  if (length(fldQ$includeFields)) {

    fldQ$selectFields <- paste0(
      sprintf("'%s', \"%s\"",
              fldQ$includeFields, fldQ$includeFields), collapse = ", ")

    if (!any("_id" == fldQ$excludeFields) &&
        !any("_id" == fldQ$includeFields)) fldQ$selectFields <- paste0(
          '\'_id\', "_id", ', fldQ$selectFields)

    fldQ$selectFields <- paste0('jsonb_build_object(', fldQ$selectFields, ')')

  } else {

    fldQ$selectFields <-
      "'{\"_id\": \"' || _id || '\", ' || LTRIM(json::TEXT, '{')"

  }

  fldQ$extractFields <- paste0(
    sprintf(", jsonb_path_query_array(json, \'$.\"%s\"') AS \"%s\"", # _array
            gsub('[.]', '"."', fldQ$includeFields[fldQ$includeFields != "_id"]),
            fldQ$includeFields[fldQ$includeFields != "_id"]),
    collapse = " ")

  fldQ$existsFields <- paste0(
    sprintf("jsonb_path_exists(json, '$.\"%s\"')",
            gsub('[.]', '"."', fldQ$includeFields[fldQ$includeFields != "_id"])), collapse = " OR ")
  if (fldQ$existsFields == "") fldQ$existsFields <- "TRUE"


  # query

  if (!length(fldQ$queryCondition)) fldQ$queryCondition <- "TRUE"

  fldQ$queryCondition <- gsub("'", '"', fldQ$queryCondition)

  for (i in fldQ$queryPaths[fldQ$queryPaths != "_id"]) {

    fldQ$queryCondition <- gsub(
      paste0("(\"", i, "\" [INOTREGXP=!<>']+ .+?)( AND | NOT | OR |\\)*$)"),
      "jsonb_path_exists(json, \'$[*] ? (@.\\1)')\\2", # keep $[*]
      fldQ$queryCondition)

    # - special case IN

    if (grepl(paste0("\"", i, "\" IN \\("), fldQ$queryCondition)) {

      # "jsonb_path_exists(json, '$[*] ? (@.\"a\" == \"b\" || @.\"a\" == \"c\")')"
      ins <- stringi::stri_replace_all_regex(
        fldQ$queryCondition,
        paste0(".*\"", i, "\" IN \\((.+?)\\)( AND | NOT | OR |\\).*$)"),
        "$1"
      )

      rep <- paste0(
        "@.\"", i, "\" == ",
        # split on comma after number or double quote, avoid splitting on comma in string
        strsplit(gsub("([0-9\"]),", "\\1@", ins), "@")[[1]],
        collapse = " || ")

      fldQ$queryCondition <- stringi::stri_replace_all_regex(
        fldQ$queryCondition,
        paste0("(jsonb_path_exists\\(json, '\\$\\[\\*\\] \\? \\(@[.]\"",
               i, "\" IN \\(.+?)\\)( AND | NOT | OR |\\).*$)"),
        paste0("jsonb_path_exists(json, '\\$[*] ? (", rep, "$2")
      )

    }

    fldQ$queryCondition <- gsub(
      paste0(i, '" REGEXP '),
      paste0(i, '" like_regex '),
      fldQ$queryCondition)

  }

  # - mangle _id in query, since this does not use jsonb_path_exists syntax
  fldQ$queryCondition <- gsub('"_id" REGEXP ', '"_id" ~ ', fldQ$queryCondition)
  fldQ$queryCondition <- gsub('"_id" (~|!=|<=|>=|<>|=)=? "(.+?)"',
                              "\"_id\" \\1 '\\2'", fldQ$queryCondition)

  # - mangle _id, since character values should be in double quotes
  all <- stringi::stri_extract_all_regex(
    fldQ$queryCondition, '"_id" IN \\((.+?)\\)')[[1]]
  if (!is.na(all)) {
    ins <- stringi::stri_replace_all_regex(
      all, '"_id" IN \\((.+?)\\)', "$1")
    rep <- strsplit(gsub("([0-9\"]),", "\\1@", ins), "@")
    rep <- sapply(rep, function(i) paste0(gsub("^\"|\"", "'", trimws(i)), collapse = ", "))
    fldQ$queryCondition <- stringi::stri_replace_all_fixed(
      fldQ$queryCondition,
      all,
      paste0('"_id" IN (', rep, ")"),
      vectorize_all = FALSE
    )}


  # statement
  statement <- insObj('
    SELECT
    /** fldQ$selectFields **/
    AS json FROM "/** key **/"
    /** fldQ$extractFields **/
    WHERE  (
    /** fldQ$existsFields **/
    ) AND (
    /** fldQ$queryCondition **/
    );')


  # early return if listfields
  if (!is.null(params$listfields)) {

    # fallback but only for listfields
    if (!length(fldQ$queryCondition)) fldQ$queryCondition <- "TRUE"

    # statement
    statement <- insObj('
      WITH RECURSIVE extracted (key, value, type) AS (
        SELECT
          NULL AS key, json AS value, \'object\'
          FROM "/** key **/"
          WHERE /** fldQ$queryCondition **/
          UNION ALL
          (
           WITH tpVal AS (
           SELECT key, jsonb_typeof(value) AS typeof, value
           FROM extracted
          )
          SELECT CONCAT_WS(\'.\', t.key, v.key), v.value, jsonb_typeof(v.value)
          FROM tpVal as t, LATERAL jsonb_each(value) v
          WHERE typeof = \'object\'
            UNION ALL
          SELECT t.key, element.val, jsonb_typeof(element.val)
          FROM tpVal as t, LATERAL
          jsonb_array_elements(value) WITH ORDINALITY as element (val, n)
          WHERE typeof = \'array\'
        )
      )
      SELECT DISTINCT key
      FROM extracted
      WHERE key IS NOT NULL
      ORDER BY key
      ;')

    return(sort(DBI::dbGetQuery(
      conn = src$con,
      statement = statement,
      n = -1L
    )[["key"]]))

  }

  # processing

  # - return
  return(
    processExcludeFields(
      jsonlite::stream_in(
        textConnection(
          jqr::jq(
            # converting to text to address:
            # jq method not implemented for pq_jsonb
            as.character(
              DBI::dbGetQuery(
                conn = src$con,
                statement = statement,
                n = n)[["json"]]),
            # script to flatten or unbox arrays of length 1
            " with_entries ( .value |= (if type == \"array\"
              and length == 1 then .[] else . end) ) ")
        ), verbose = FALSE
      ), fldQ$excludeFields)
  )

}


#' @export
docdb_query.src_duckdb <- function(src, key, query, ...) {


  # make dotted parameters accessible
  params <- list(...)


  # add limit if not in params
  n <- -1L
  if (!is.null(params$limit)) n <- params$limit


  # query
  query <- jsonlite::minify(query)


  # add fields if not in params
  fields <- "{}"
  if (!is.null(params$fields)) fields <- params$fields


  # digest and mangle
  fldQ <- digestFields(f = fields, q = query)


  # - early return if only _id is requested
  if (query == "{}" &&
      length(fldQ$includeFields) == 1L &&
      fldQ$includeFields == "_id") {

    statement <- paste0(
      "SELECT DISTINCT ON (_id) _id ",
      "FROM \"", key, "\" GROUP BY _id;")

    return(DBI::dbGetQuery(
      conn = src$con,
      n = n, statement = statement))

  }


  # fields

  fldQ$extractFields <- unique(c(fldQ$includeRootFields, fldQ$queryRootFields))
  fldQ$extractFields <- fldQ$extractFields[fldQ$extractFields != "_id"]

  if (length(fldQ$extractFields)) {

    fldQ$extractFields <- paste0(
      ", json_extract(json, ['",
      paste0(fldQ$extractFields, collapse = "', '"),
      "']) extracted_list, ",
      paste0("extracted_list[", seq_along(fldQ$extractFields),
             "] AS ", fldQ$extractFields, collapse = ", "
      ))

  }

  fldQ$selectFields <- unique(c(fldQ$includeRootFields, fldQ$queryRootFields))
  fldQ$selectFields <- paste0("json_object('_id', _id, ", paste0(
    sprintf("'%s', %s", fldQ$selectFields, fldQ$selectFields), collapse = ", "), ")")

  fldQ$selectCondition <- unique(c(fldQ$includeRootFields, fldQ$queryRootFields))
  if (length(fldQ$selectCondition)) fldQ$selectCondition <-
    paste0("(", fldQ$selectCondition, " <> '{}')", collapse = " OR ")


  # query

  # - if all query fields are top level elements
  if (length(fldQ$queryFields) &&
      identical(fldQ$queryFields, fldQ$queryRootFields)) {

    # - duckdb expects quotes around string values
    #   from json, type casting did not work / help
    for (i in fldQ$queryFields) { #[fldQ$queryFields != "_id"]) {

      fldQ$queryCondition <- gsub(
        # '(.+?)' shall not capture numbers, not IN('A', 'B')
        paste0("\"(", i, ")\" ([INOTREGXP=!<>']+) '(.+?)'( AND | NOT | OR |\\)*$)"),
        paste0('\\1 \\2 \'', ifelse(i == "_id", '\\3', '"\\3"'), '\' \\4'),
        fldQ$queryCondition
      )
    }

    # - regexp
    for (i in fldQ$queryFields) {

      ins <- stringi::stri_extract_all_regex(
        # note subsequent to above no double quotes around i
        fldQ$queryCondition, paste0("(", i, ") REGEXP '\"?([^\"]+?)\"?'"))[[1]]

      if (!is.na(ins)) {

        ii <- stringi::stri_replace_all_regex(
          ins, paste0("(", i, ") REGEXP '\"?([^\"]+?)\"?'"), "$2")

        # special handling of _id as not coming from json

        # replace anchors ^ and $ with double quotes as new anchors;
        # for ARRAY elements, strings (have to be in double quotes)
        if (i != "_id") ii <- gsub("\\^|\\$", '"', ii)

        ii <- paste0("regexp_matches(", i, ", '", ii, "')")

        fldQ$queryCondition <- stringi::stri_replace_all_fixed(
          fldQ$queryCondition, ins, ii, vectorize_all = TRUE)

      }
    }

    # - in. mongo: the $in operator selects documents where the
    #   value of a field equals any value in the specified array
    for (i in fldQ$queryFields[fldQ$queryFields != "_id"]) {

      ins <- stringi::stri_extract_all_regex(
        fldQ$queryCondition, paste0("\"", i, "\" IN \\((.+?)\\)"))[[1]]

      if (!is.na(ins)) {

        isString <- grepl("\\('", ins) # "5, 4" or "'a ,b', 'd, f'"
        ii <- stringi::stri_replace_all_regex(ins, "\"(.+?)\" IN \\((.+?)\\)", "$2")
        ii <- stringi::stri_split_regex(ii, ifelse(isString, "', ", ", "))[[1]]
        ii <- stringi::stri_replace_all_regex(ii, "^'|'$", "")
        ii <- paste0("json_contains(", i, ", '", ifelse(isString, '"', ""),
                     ii, ifelse(isString, '"', ""), "')")
        ii <- paste0("(", paste0(ii, collapse = " OR "), ")")

        fldQ$queryCondition <- stringi::stri_replace_all_fixed(
          fldQ$queryCondition, ins, ii, vectorize_all = TRUE
        )
      }
    }

    # - integrate
    fldQ$selectCondition <- paste0(
      "( ( ", fldQ$selectCondition, " ) AND ", fldQ$queryCondition, ")")

    # - reset
    fldQ$queryCondition <- character(0L)

  }

  # - empty query
  if (!length(fldQ$queryCondition) &&
      !length(fldQ$selectCondition)) fldQ$selectCondition <- "TRUE"

  # - if query needs jqr
  fldQ$jqrWhere <- character(0L)
  if (length(fldQ$queryCondition)) {

    fldQ$jqrWhere <- fldQ$queryJq

  }


  # - query if fields == '{}' or
  #   if jqrWhere is needed
  if (!length(fldQ$includeFields) ||
      length(fldQ$jqrWhere)) {

    fldQ$extractFields <- paste0(", json", fldQ$extractFields)
    fldQ$selectFields <- "\'{\"_id\": \"\' || _id || \'\", \' || LTRIM(json, \'{\')"

  }

  # - query if fields == '{"_id":1}' and
  #   jqrWhere is not needed
  if (length(fldQ$includeFields) == 1L &&
      fldQ$includeFields == "_id" &&
      !length(fldQ$jqrWhere)) {

    fldQ$selectFields <- "\'{\"_id\": \"\' || _id || \'\"}\'"
    fldQ$includeFields <- character(0L)

  }


  # compose statement
  statement <- insObj('
    WITH extracted AS (
    SELECT _id
    /** fldQ$extractFields **/
    FROM "/** key **/" )
    SELECT /** fldQ$selectFields **/
    AS json FROM extracted
    WHERE  (
    /** fldQ$selectCondition **/
    );')


  # special case: return all fields if listfields != NULL
  if (!is.null(params$listfields)) {

    # fallback but only for listfields
    statement <- insObj('
    WITH extracted AS (
    SELECT _id
    /** fldQ$extractFields **/
    FROM "/** key **/" )
    SELECT _id, json
    FROM extracted
    WHERE  (
    /** fldQ$selectCondition **/
    );')

    j <- 'path(..) | join (".") '
    fldQ$jqrWhere <- ifelse(
      length(fldQ$jqrWhere), paste0(fldQ$jqrWhere, " | ", j), j)

    fields <- processDbGetQuery(
      getData = 'paste0(DBI::dbGetQuery(conn = src$con,
                 statement = statement, n = n)[["json"]], "")',
      jqrWhere = fldQ$jqrWhere)[["out"]]

    # mangle "friends.0", "friends.0.id"
    fields <- unique(gsub("[.][0-9]+", "", fields))
    fields <- fields[fields != "_id"]
    fields <- fields[fields != ""]

    # return field names
    return(sort(fields))

  }

  # general processing
  return(processDbGetQuery(
    getData = 'paste0(DBI::dbGetQuery(conn = src$con,
               statement = statement, n = n)[["json"]], "")',
    jqrWhere = fldQ$jqrWhere,
    outputFields = fldQ$includeFields,
    excludeFields = fldQ$excludeFields))

}


## helpers --------------------------------------



#' processDbGetQuery
#'
#' @importFrom R.utils countLines
#' @keywords internal
#' @noRd
#'
processDbGetQuery <- function(
    getData,
    jqrWhere = character(0L),
    outputFields = character(0L),
    excludeFields = character(0L)) {


  # debug
  if (options()[["verbose"]]) {
    eval.parent(parse(text = 'if (exists("statement")) message("\nSQL: ", statement, "\n")'))
    eval.parent(parse(text = 'if (exists("query")) message("\nDB: ", query, "\n")'))
  }

  # temporary file and connection
  tfname <- tempfile()
  on.exit(try(unlink(tfname), silent = TRUE), add = TRUE)


  #### .write dbGetQuery ####

  # process and early exit
  if (!length(jqrWhere) &&
      !length(outputFields) &&
      !length(excludeFields)) return(
        jsonlite::stream_in(
          textConnection(
            eval.parent(parse(text = getData))),
          verbose = FALSE))

  # write out
  writeLines(
    eval.parent(parse(text = getData)),
    con = tfname)

  # early exit
  if (file.size(tfname) <= 1L) return(NULL)

  # debug
  if (options()[["verbose"]]) {
    message("NDJSON lines: ", R.utils::countLines(tfname), "\n")
  }


  #### .jq WHERE ####

  # use jq to implement WHERE
  if (length(jqrWhere)) {

    # debug
    if (options()[["verbose"]]) {
      message("JQ WHERE: ", jqrWhere, "\n")
    }

    # process
    if (!length(outputFields) &&
        !length(excludeFields)) return(
          jsonlite::stream_in(
            textConnection(
              jqr::jq(file(tfname), jqrWhere)),
            verbose = FALSE))

    # get another file name
    tjname <- tempfile()
    on.exit(try(unlink(tjname), silent = TRUE), add = TRUE)

    # write data
    jqr::jq(
      file(tfname),
      jqrWhere,
      flags = jqr::jq_flags(pretty = FALSE),
      out = tjname
    )

    # early exit
    if (!file.size(tjname)) return(NULL)

    # swap file name
    tfname <- tjname

  } # !is.null(jqrWhere)


  #### .jq outputFields ####

  # only if outputFields is defined
  if (length(outputFields)) {

    # debug
    if (options()[["verbose"]]) {
      message("outputFields: ", paste0(outputFields, collapse = " / "), "\n")
    }

    # early exit
    if (!length(excludeFields)) return(
      processOutputFields(tfname, outputFields))

    # get another file name
    tjname <- tempfile()
    on.exit(try(unlink(tjname), silent = TRUE), add = TRUE)

    # write data
    processOutputFields(tfname, outputFields, tjname)

    # early exit
    if (!file.size(tjname)) return(NULL)

    # swap file name for next processing step
    tfname <- tjname

  } # if (length(outputFields))


  #### .jq excludeFields ####
  if (length(excludeFields)) {

    # process
    return(
      processExcludeFields(
        jsonlite::stream_in(
          file(tfname),
          verbose = FALSE),
        excludeFields)
    )

  } # if (!length(excludeFields))

  # unexpected
  stop("unforeseen branching in processDbGetQuery")

}



#' processExcludeFields
#'
#' @keywords internal
#' @noRd
#'
processExcludeFields <- function(df, excludeFields) {

  # early return
  if (!length(excludeFields)) return(df)
  if (!nrow(df)) return(df)

  # exclude any root fields with name:0 or that were not specified
  rM <- stats::na.omit(match(excludeFields, names(df)))

  # process
  if (length(rM)) return(df[, -rM, drop = FALSE])

  # alternative return
  return(df)

}



#' processOutputFields
#'
#' @keywords internal
#' @noRd
#'
processOutputFields <- function(tfname, outputFields, tjname = NULL) {

  # create output json with dot field names that
  # represent nested fields and their values

  # input:
  # {"_id", "friends": {"id": [."friends" | (if type != "array" then [.] else .[] end) | ."id"] }}
  #
  # script:
  # "{\"_id\", \"friends.id\": [.\"friends\" | (if type != \"array\" then [.][] else .[] end) | .\"id\"]}"
  #
  # output:
  # {"_id", "friends.id": [1, 2, 3]}

  subFields <- strsplit(outputFields, split = "[.]")
  rootFields <- subFields[sapply(subFields, length) == 1L]
  subFields <- subFields[sapply(subFields, length) > 1L]

  jqFields <- ifelse(!length(rootFields), '"_id"', paste0(
    # keep _id even if not specified, can be
    # removed with _id:0 in subsequent step
    '"', unique(c("_id", rootFields)), '"', collapse = ", "))

  if (jqFields != "") jqFields <- paste0(jqFields, ", ", collapse = "")

  jqFields <- paste0(
    'def m1: . | (if length == 0 then null else (if type != "array" then [.][] else .[] end) end); ',
    'def m2: . | (if length > 1 then [.][] else .[] end); {',
    paste0(c(jqFields, paste0(
      sapply(
        subFields,
        function(s) {
          # first item
          k <- paste0('"', s[1], '')
          v <- paste0(' [."', s[1], '" | m1 | ')
          # next item(s)
          for (i in (seq_len(length(s) - 2) + 1)) {
            k <- paste0(k, '.', s[i], '')
            v <- paste0(v, '."', s[i], '" | m1 | ')
          }
          # last item
          k <- paste0(k, '.', s[length(s)], '": ')
          v <- paste0(v, '."', s[length(s)], '"] | m2 ')
          # combine item(s)
          paste0(k, v)
        }, USE.NAMES = FALSE),
      collapse = ", ")), collapse = ""), "}")

  # debug
  if (options()[["verbose"]]) {
    message("JQ processOutput: ", jqFields, "\n")
  }

  # early return
  if (is.null(tjname)) return(
    jsonlite::stream_in(
      textConnection(
        jqr::jq(file(tfname), jqFields)),
      verbose = FALSE))

  # write data for further processing
  jqr::jq(
    file(tfname),
    jqFields,
    out = tjname
  )

  return(NULL)

} # processOutputFields
