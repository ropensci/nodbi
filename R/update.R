#' Update documents
#'
#' Documents are updated by patching their JSON with
#' \code{value}.
#'
#' Documents are identified by the \code{value} or
#' by _id's in \code{value}, where the latter takes
#' precedence in case both are specified.
#'
#' \code{value} can have multiple documents and _id's,
#' which then are used for iterative updating.
#'
#' This is native with MongoDB, SQLite and DuckDB.
#' It uses a plpgsql function added to PostgreSQL.
#' For Elasticsearch and CouchDB, jq is used.
#'
#' @inheritParams docdb_create
#'
#' @param query (character) A JSON query string, see examples
#'
#' @param ... Passed on to functions:
#' - CouchDB: [sofa::db_bulk_create()]
#' - Elasticsearch: [elastic::docs_bulk_update]
#' - MongoDB: [mongolite::mongo()]
#' - SQLite: ignored
#' - PostgreSQL: ignored
#' - DuckDB: ignored
#'
#' @return (integer) Number of successfully updated documents
#'
#' @export
#'
#' @examples \dontrun{
#' src <- src_sqlite()
#' docdb_create(src, "mtcars", mtcars)
#' docdb_update(src, "mtcars", value = mtcars[3, 4:5], query = '{"gear": 3}')
#' docdb_update(src, "mtcars", value = '{"carb":999}', query = '{"gear": 5}')
#' docdb_update(src, "mtcars", value = '{"_id":"Fiat 128", carb":999}', query = '')
#' docdb_get(src, "mtcars")
#' }
docdb_update <- function(src, key, value, query, ...) {
  assert(src, "docdb_src")
  assert(key, "character")
  assert(value, c("data.frame", "list", "character"))
  assert(query, "character")
  UseMethod("docdb_update", src)
}

#' @export
docdb_update.src_couchdb <- function(src, key, value, query, ...) {

  # If you want to change a document in CouchDB, you don’t tell it to go and
  # find a field in a specific document and insert a new value. Instead,
  # you load the full document out of CouchDB, make your changes in the
  # JSON structure (or object, when you are doing actual programming),
  # and save the entire new revision (or version) of that document back into CouchDB.

  # get original set
  input <- docdb_query(src, key, query)

  # early return if not found
  if (!length(input)) return(0L)

  # original set data frame to json
  ndjson <- NULL
  jsonlite::stream_out(input, con = textConnection(
    object = "ndjson", open = "w", local = TRUE), verbose = FALSE)

  # data frame to json
  if (all(class(value) %in% "data.frame")) {
    row.names(value) <- NULL
    value <- jsonlite::toJSON(value, dataframe = "columns", auto_unbox = TRUE)
  }
  # list to json
  if (all(class(value) %in% "list")) {
    value <- jsonlite::toJSON(value, auto_unbox = TRUE)
  }

  # merge json with value
  value <- jqr::jq(paste0(
    "[",  jqr::jq(textConnection(ndjson)),
    ",", value, "]"), ' reduce .[] as $item ({}; . * $item) ')

  # jqr output to list
  value <- jsonlite::stream_in(
    textConnection(value),
    simplifyVector = FALSE,
    verbose = FALSE)

  # note: sofa::db_bulk_update changes all
  # documents in the container, therefore
  # - delete documents
  invisible(docdb_delete(src, key, query = query, ...))
  # - create documents
  result <- suppressMessages(
    docdb_create(src, key, value, ...))

  # return
  return(result)

}

#' @export
docdb_update.src_elastic <- function(src, key, value, query, ...) {

  # get _id's
  ids <- docdb_query(src, key, query, fields = '{"_id": 1}')[["_id"]]

  # early return if not found
  if (!length(ids)) return(0L)

  # process value, target is data frame

  # json to data frame
  if (all(class(value) %in% "character")) {
    value <- jsonlite::fromJSON(value)
  }

  # list to data frame
  if (all(class(value) %in% "list")) {
    value <- jsonlite::fromJSON(
      jsonlite::toJSON(value, auto_unbox = TRUE))
    # if value is still simple list
    value <- as.data.frame(value)
  }

  # data frame
  row.names(value) <- NULL

  # how to handle?
  if (nrow(value) != length(ids)) {
    # expanding data frame as needed to match _id's
    indf <- value
    value <- NULL
    #
    if (nrow(indf) == 1L) {
      for (i in seq_len(length(ids))) {
        value <- rbind(value, indf)
      }}
    #
    if (nrow(indf) > 1L) {
      # iterating over _id's
      for (i in seq_len(length(ids))) {
        value <- rbind(value, t(cbind(as.list(indf))))
      }
      value <- as.data.frame(value)
    }
  }

  # Error: no 'docs_bulk_update' method for list
  # therefore using only data frame for update
  result <- elastic::docs_bulk_update(
    conn = src$con,
    index = key,
    x = value,
    doc_ids = ids,
    quiet = TRUE,
    digits = NA,
    ...
  )

  # prepare return value
  if (inherits(result, "try-error") || any(sapply(result, "[[", "errors"))) {
    error <- unlist(result, use.names = TRUE)
    error <- error[grepl("caused_by[.]reason$", names(error))]
    warning(
      "Could not create some documents, reason: ",
      unique(error), call. = FALSE, immediate. = TRUE)
  }
  result <- unlist(result, use.names = TRUE)
  result <- result[names(result) == "items.update.result"]
  if (any(result != "updated")) {
    warning("Could not create some documents, reason: ",
            unique(result[result != "updated"]), call. = FALSE, immediate. = TRUE)
  }
  result <- sum(result == "updated")
  return(result)

}

#' @export
docdb_update.src_mongo <- function(src, key, value, query, ...) {

  # special check for mongo
  chkSrcMongo(src, key)

  # if regexp query lacks options, add them in
  if (grepl('"[$]regex" *: *"[^,$:}]+?" *}', query)) query <-
      sub('("[$]regex" *: *"[^,$:}]+?" *)', '\\1, "$options": ""', query)

  # process value, target is json string

  # check other inputs
  if (isFile(value)) {
    value <- jqr::jq(file(value), flags = jqr::jq_flags(pretty = FALSE))
  } else if (isUrl(value)) {
    value <- jqr::jq(url(value), flags = jqr::jq_flags(pretty = FALSE))
  }

  # handle potential json string input
  if (length(value) == 1 && is.atomic(value) &&
      is.character(value) && jsonlite::validate(value)
      ) {
    # check format
    if (all(jqr::jq(value, " .[] | type ") == '"array"') & length(jqr::jq(value, " .[] ")) > 1L) stop(
      "Require JSON string that is an array of documents, not a set of fields that are arrays."
    )
    # check if top level is an array
    chk <- jqr::jq(value, " type ")
    if (length(chk) == 1L && chk == '"array"') value <- jqr::jq(value, " .[] ")
  }

  # data frame to json
  if (all(class(value) %in% "data.frame")) {
    # if value contains id's, split rows into documents of a vector
    row.names(value) <- NULL
    if (any(names(value) == "_id")) {
      value <- jsonlite::toJSON(value, dataframe = "rows", auto_unbox = TRUE)
      value <- jqr::jq(value, ' .[] ')
    } else {
      # otherwise keep as single document
      value <- jsonlite::toJSON(value, dataframe = "columns", auto_unbox = TRUE)
    }
  }

  # list to json
  if (all(class(value) %in% "list")) {
    value <- jsonlite::toJSON(value, dataframe = "rows", auto_unbox = TRUE)
    # check if top level is an array
    chk <- jqr::jq(value, " type ")
    if (length(chk) == 1L && chk == '"array"') value <- jqr::jq(value, " .[] ")
  }

  # get doc ids to update
  # bulk update: if ids are in value, query is ignored
  ids <- try(jqr::jq(value, ' ._id '), silent = TRUE)
  if (inherits(ids, "try-error")) ids <- NULL
  ids <- gsub("\"", "", as.character(ids))
  ids <- ids[ids != "null"]
  if (length(ids)) {
    if (query != "") warning(
      "Ignoring the specified 'query' parameter, using _id's ",
      "found in 'value' to identify documents to be updated")
    value <- jqr::jq(value, ' del(._id) ')
    ids <- paste0('{"_id":"', ids, '"}')
  } else {
    if (length(value) > 1L) {
      # find documents to be updated for each document in value
      ids <- paste0('{"_id":"', suppressWarnings(
        src$con$find(query = query, fields = '{"_id":1}', ...))[["_id"]], '"}')
    } else {
      # keep original query if only a single document is in value
      ids <- query
    }
  }

  # check
  if (length(value) > 1L && (length(value) != length(ids))) stop(
    "Unequal number of documents identified (", (length(ids)),
    ") and of documents in 'value' (", length(value), ")"
  )

  # turn into json set
  value <- vapply(
    X = value,
    function(i) {paste0('{"$set":', i, "}")},
    FUN.VALUE = character(1L),
    USE.NAMES = FALSE)

  # iterate over data
  result <- 0L
  for (i in seq_along(value)) {

    # do update
    res <- try(
      suppressWarnings(
        src$con$update(
          query = ids[i],
          update = value[i],
          upsert = FALSE,
          multiple = TRUE,
          ...))[c("matchedCount", "upsertedCount")],
      silent = TRUE)

    # accumulate
    result <- c(result, res)

  }

  # generate user info
  result <- unlist(result)
  if (inherits(result, "try-error") ||
      any(grepl("error", result, ignore.case = TRUE))) {
    error <- result[grepl("rror", result)]
    error <- trimws(sub(".+E[0-9]+(.*?):.+", "\\1", error))
    warning(
      "Could not create some documents, reason: ",
      unique(error), call. = FALSE, immediate. = TRUE)
    result <- result[!grepl("rror", result)]
    result <- min(0, as.integer(result))
  }
  return(sum(result))

}

#' @export
docdb_update.src_sqlite <- function(src, key, value, query, ...) {

  # SQL for patching, see https://www.sqlite.org/json1.html#jpatch
  updFunction <- "json_patch"

  return(sqlUpdate(src = src, key = key, value = value, query = query, updFunction = updFunction))

}

#' @export
docdb_update.src_postgres <- function(src, key, value, query, ...) {

  # Since PostgreSQL has no internal function,
  # uses function inserted by nodbi::src_postgres
  updFunction <- "jsonb_merge_patch"

  return(sqlUpdate(src = src, key = key, value = value, query = query, updFunction = updFunction))

}

#' @export
docdb_update.src_duckdb <- function(src, key, value, query, ...) {

  # see https://duckdb.org/docs/extensions/json#json-creation-functions
  updFunction <- "json_merge_patch"

return(sqlUpdate(src = src, key = key, value = value, query = query, updFunction = updFunction))

}

## helpers --------------------------------------

#' @keywords internal
#' @noRd
sqlUpdate <- function(src, key, value, query, updFunction) {

  # check other inputs
  # note value can now be a vector
  if (isFile(value)) {
    value <- jqr::jq(file(value), flags = jqr::jq_flags(pretty = FALSE))
  } else if (isUrl(value)) {
    value <- jqr::jq(url(value), flags = jqr::jq_flags(pretty = FALSE))
  }

  # handle potential json string input
  if (length(value) == 1 && is.atomic(value) &&
      is.character(value) && jsonlite::validate(value)) {
    # check format
    if (all(jqr::jq(value, " .[] | type ") == '"array"') & length(jqr::jq(value, " .[] ")) > 1L) stop(
      "Require JSON string that is an array of documents, not a set of fields that are arrays."
    )
    # check if top level is an array
    chk <- jqr::jq(value, " type ")
    if (length(chk) == 1L && chk == '"array"') value <- jqr::jq(value, " .[] ")
  }

  # data frame to json
  if (all(class(value) %in% "data.frame")) {
    # if value contains id's, split rows into documents of a vector
    row.names(value) <- NULL
    if (any(names(value) == "_id")) {
      value <- jsonlite::toJSON(value, dataframe = "rows", auto_unbox = TRUE)
      value <- jqr::jq(value, ' .[] ')
    } else {
      # otherwise keep as single document
      value <- jsonlite::toJSON(value, dataframe = "columns", auto_unbox = TRUE)
    }
  }

  # list to json
  if (all(class(value) %in% "list")) {
    value <- jsonlite::toJSON(value, auto_unbox = TRUE)
    # check if top level is an array
    chk <- jqr::jq(value, " type ")
    if (length(chk) == 1L && chk == '"array"') value <- jqr::jq(value, " .[] ")
  }

  # get doc ids to update
  # bulk update: if ids are in value, query is ignored
  ids <- try(jqr::jq(value, ' ._id '), silent = TRUE)
  if (inherits(ids, "try-error")) ids <- NULL
  ids <- gsub("\"", "", as.character(ids))
  ids <- ids[ids != "null"]
  if (length(ids)) {
    if (query != "") warning(
      "Ignoring the specified 'query' parameter, using _id's ",
      "found in 'value' to identify documents to be updated")
    value <- jqr::jq(value, ' del(._id) ')
  } else {
    ids <- docdb_query(src, key, query, fields = '{"_id": 1}')[["_id"]]
  }

  # check
  if (!length(ids)) return(0L)
  if (length(value) > 1L && (length(value) != length(ids))) stop(
    "Unequal number of documents identified (", (length(ids)),
    ") and of documents in 'value' (", length(value), ")"
  )

  # default case, update with one set:
  # replace ids vector with atomic ids string
  if (length(value) == 1L) ids <- sub(
    "'(.+)'", "\\1", paste0(
      "'", ids, "'", collapse = ","))

  # iterate over data
  result <- 0L
  for (i in seq_along(value)) {

    # compose statement
    statement <- paste0(
      'UPDATE "', key, '" SET json = ', updFunction, '(json,\'',
      value[i], '\') WHERE _id IN (\'', ids[i], '\');'
    )

    # update data
    result <- result + try(
      DBI::dbWithTransaction(
        conn = src$con,
        code = {
          DBI::dbExecute(
            conn = src$con,
            statement = statement
          )}), silent = TRUE)

  } # for

  # return
  return(result)

}


