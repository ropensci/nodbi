#' Update documents
#'
#' Documents are updated by patching their JSON with
#' \code{value}. Documents are identified by a \code{query}
#' or by `_id`'s in \code{value}, where the latter takes
#' precedence. \code{value} can have multiple documents (with
#' `_id`'s), which then are iteratively updated.
#'
#' Uses native functions in MongoDB ([mongolite::mongo()]$update()),
#' SQLite (`jsonb_update()`), DuckDB (`jsonb_merge_patch()`),
#' Elasticsearch (`elastic::docs_bulk_update()`);
#' a `plpgsql` function added when calling `src_postgres()`,
#' and a [jqr] programme for CouchDB.
#'
#' @inheritParams docdb_create
#'
#' @param query (character) A JSON query string, see examples.
#'  Can use comparisons / tests (`$lt`, `$lte`, `$gt`, `$gte`,
#'  `$ne`, `$in`, `$regex`), with logic operators (`$and`,
#'  `$or`, `(`, `)`), including nested queries, see examples.
#'
#' @param ... Passed on to functions [elastic::docs_bulk_update()],
#' and [mongolite::mongo()]$update().
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
#' docdb_update(src, "mtcars", value = '{"_id":"Fiat 128", "carb":888}', query = '{}')
#' docdb_get(src, "mtcars")
#' }
docdb_update <- function(src, key, value, query, ...) {
  assert(src, "docdb_src")
  assert(key, "character")
  assert(value, c("data.frame", "list", "character"))
  assert(query, c("json", "character"))

  if (query == "") {
    warning('query = "" is deprecated, use query = "{}"')
    query <- "{}"
  }
  stopifnot(jsonlite::validate(query))

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
  query <- jsonlite::minify(query)

  # process value, target is json strings in file

  # check other inputs
  if (isFile(value)) {
    value <- jqr::jq(file(value), ".", flags = jqr::jq_flags(pretty = FALSE))
  } else if (isUrl(value)) {
    value <- jqr::jq(url(value), ".", flags = jqr::jq_flags(pretty = FALSE))
  }

  # handle potential json string input
  if (length(value) == 1 && is.atomic(value) &&
      is.character(value) && jsonlite::validate(value)
  ) {
    # check format
    if (all(jqr::jq(value, " .[] | type ") == '"array"') && length(jqr::jq(value, " .[] ")) > 1L) stop(
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
      value <- jsonlite::toJSON(value, dataframe = "rows", auto_unbox = TRUE, digits = NA)
      value <- jqr::jq(value, " .[] ")
    } else {
      # otherwise keep as single document
      value <- jsonlite::toJSON(value, dataframe = "columns", auto_unbox = TRUE, digits = NA)
    }
  }
  # list to json
  if (all(class(value) %in% "list")) {
    value <- jsonlite::toJSON(value, auto_unbox = TRUE, digits = NA)
    # check if {"_id":["Valiant","Fiat 128"],"gear":[8,9]}
    if (all(jqr::jq(value, '._id | type' ) == '"array"')) {
      value <- data.frame(jsonlite::fromJSON(value), check.names = FALSE)
      value <- jsonlite::toJSON(value)
    }
    # check if top level is an array
    chk <- jqr::jq(value, " type ")
    if (length(chk) == 1L && chk == '"array"') value <- jqr::jq(value, " .[] ")
  }

  # get _id's of docs to be updated
  valueIds <- gsub(
    '"', "", as.character(jqr::jq(textConnection(value), '._id | select(. != null)'
    )))

  # early return if none found
  if (query == "{}" && !length(valueIds)) return(0L)

  # check
  if (query != "{}" && length(valueIds)) warning(
    "Ignoring the specified 'query' parameter, using _id's ",
    "found in 'value' to identify documents to be updated")

  # get docs to be updated
  if (length(valueIds)) query <- paste0(
    '{"_id": {"$in": [', paste0('"', valueIds, '"', collapse = ", "), ']}}')
  #
  input <- docdb_query(src, key, query)
  if (!length(input)) return(0L)
  if (!length(valueIds)) valueIds <- input[["_id"]]
  #
  ndjson <- NULL
  jsonlite::stream_out(input, con = textConnection(
    object = "ndjson", open = "w", local = TRUE), verbose = FALSE, digits = NA)

  # temporary file and connection
  tfname <- tempfile()
  tfnameCon <- file(tfname, open = "at")
  # register to close and remove file after used for streaming
  on.exit(try(unlink(tfname), silent = TRUE), add = TRUE)
  on.exit(try(close(tfnameCon), silent = TRUE), add = TRUE)

  # check
  if (length(value) > 1L && !identical(nrow(input), length(value))) stop(
    "Unequal number of documents identified (", nrow(input),
    ") and of documents in 'value' (", length(value), ")"
  )

  # merge and append to file
  for (i in seq_along(valueIds)) {

    idValue <- jqr::jq(
      textConnection(value),
      paste0('select(._id == "', valueIds[i], '")'))

    if (!length(idValue)) {
      if (length(value) > 1L) {
        idValue <- value[i]
      } else {
        idValue <- value
      }
    }

    # merge input json with value
    jqr::jq(
      x = textConnection(paste0(
        "[", jqr::jq(
          textConnection(ndjson),
          paste0("select(._id == \"", valueIds[i], "\")")),
        ",", idValue, "]")),
      " reduce .[] as $item ({}; . * $item) ",
      out = tfnameCon)

  }

  close(tfnameCon)

  # note: sofa::db_bulk_update would change all
  # documents in the container, therefore do:
  # - delete documents
  invisible(docdb_delete(src, key, query = query, ...))
  # - create documents
  result <- suppressMessages(
    docdb_create(src, key, tfname, ...))

  # return
  return(result)

}

#' @export
docdb_update.src_elastic <- function(src, key, value, query, ...) {

  # process value, target is data frame
  query <- jsonlite::minify(query)

  # needed because target is data frame
  # but data frame could be serialised
  # into a single json string line, in
  # contrast to other inputs
  valueClass <- class(value)

  # json to data frame
  if (any(class(value) == "character")) {
    if (isFile(value)) {
      value <- jsonlite::stream_in(file(value), verbose = FALSE)
    } else {
      if (isUrl(value)) {
        value <- jsonlite::stream_in(url(value), verbose = FALSE)
      } else {
        value <- jsonlite::stream_in(textConnection(value), verbose = FALSE)
      }
    }
  }

  # list to data frame
  if (all(class(value) %in% "list")) {
    value <- jsonlite::fromJSON(
      jsonlite::toJSON(value, auto_unbox = TRUE, digits = NA))
    # if value is still simple list
    value <- as.data.frame(value, check.names = FALSE)
  }

  # data frame
  row.names(value) <- NULL
  if (any(names(value) == "X_id")) names(value)[names(value) == "X_id"] <- "_id"

  # _id in data frame?
  if (any(names(value) == "_id")) {

    ids <- value[, "_id", drop = TRUE]
    value <- value[, -match("_id", names(value)), drop = FALSE]

    if (query != '{}') warning(
      "Ignoring the specified 'query' parameter, using _id's ",
      "found in 'value' to identify documents to be updated")

  } else {

    ids <- docdb_query(src, key, query, fields = '{"_id": 1}')[["_id"]]
    if (!length(ids)) return(0L)

  }

  # check
  if (!all(valueClass %in% "data.frame") &&
      nrow(value) > 1L && !identical(nrow(value), length(ids))) stop(
    "Unequal number of documents identified (", length(ids),
    ") and of documents in 'value' (", nrow(value), ")"
  )

  # how to handle?
  if (nrow(value) != length(ids)) {
    # expanding data frame as needed to match _id's
    indf <- value
    value <- NULL
    #
    if (nrow(indf) == 1L) {
      for (i in seq_len(length(ids))) {
        value <- rbind(value, indf)
      }
    }
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
    query = list(refresh = TRUE),
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
  if (any(result != "updated" & result != "noop")) {
    warning("Could not create some documents, reason: ",
            unique(result[result != "updated" & result != "noop"]),
            call. = FALSE, immediate. = TRUE)
  }
  result <- sum(result == "updated" | result == "noop")
  return(result)

}

#' @export
docdb_update.src_mongo <- function(src, key, value, query, ...) {

  # special check for mongo
  chkSrcMongo(src, key)
  query <- jsonlite::minify(query)

  # if regexp query lacks options, add them in
  if (grepl('"[$]regex" *: *"[^,$:}]+?" *}', query)) query <-
    sub('("[$]regex" *: *"[^,$:}]+?" *)', '\\1, "$options": ""', query)

  # process value, target is json string

  # check other inputs
  if (isFile(value)) {
    value <- jqr::jq(file(value), ".", flags = jqr::jq_flags(pretty = FALSE))
  } else if (isUrl(value)) {
    value <- jqr::jq(url(value), ".", flags = jqr::jq_flags(pretty = FALSE))
  }

  # handle potential json string input
  if (length(value) == 1 && is.atomic(value) &&
      is.character(value) && jsonlite::validate(value)
  ) {
    # check format
    if (all(jqr::jq(value, " .[] | type ") == '"array"') && length(jqr::jq(value, " .[] ")) > 1L) stop(
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
      value <- jsonlite::toJSON(value, dataframe = "rows", auto_unbox = TRUE, digits = NA)
      value <- jqr::jq(value, " .[] ")
    } else {
      # otherwise keep as single document
      value <- jsonlite::toJSON(value, dataframe = "columns", auto_unbox = TRUE, digits = NA)
    }
  }

  # list to json
  if (all(class(value) %in% "list")) {
    value <- jsonlite::toJSON(value, dataframe = "rows", auto_unbox = TRUE, digits = NA)
    # check if top level is an array
    chk <- jqr::jq(value, " type ")
    if (length(chk) == 1L && chk == '"array"') value <- jqr::jq(value, " .[] ")
  }

  # get doc ids to update
  # bulk update: if ids are in value, query is ignored
  ids <- try(jqr::jq(value, " ._id "), silent = TRUE)
  if (inherits(ids, "try-error")) ids <- NULL
  ids <- gsub("\"", "", as.character(ids))
  ids <- ids[ids != "null"]
  if (length(ids)) {
    if (query != "{}") warning(
      "Ignoring the specified 'query' parameter, using _id's ",
      "found in 'value' to identify documents to be updated")
    value <- jqr::jq(value, " del(._id) ")
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
    "Unequal number of documents identified (", length(ids),
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

  query <- jsonlite::minify(query)

  # SQL for patching, see https://www.sqlite.org/json1.html#jpatch
  updFunction <- "jsonb_patch"

  return(sqlUpdate(src = src, key = key, value = value, query = query, updFunction = updFunction))

}

#' @export
docdb_update.src_postgres <- function(src, key, value, query, ...) {

  query <- jsonlite::minify(query)

  # Since PostgreSQL has no internal function,
  # uses function inserted by nodbi::src_postgres
  updFunction <- "jsonb_merge_patch"

  return(sqlUpdate(src = src, key = key, value = value, query = query, updFunction = updFunction))

}

#' @export
docdb_update.src_duckdb <- function(src, key, value, query, ...) {

  query <- jsonlite::minify(query)

  # use file based approach
  if (isFile(value) && (query == "{}")) {

    statement <- paste0(
      'UPDATE "', key, '"
       SET json = json_merge_patch(json, injson)
       FROM (SELECT
        json->>\'$._id\' AS in_id,
        json_merge_patch(json, \'{\"_id\": null}\') AS injson
        FROM read_ndjson_objects("', value, '")
      ) WHERE "', key, '"._id = in_id;'
    )

    result <- DBI::dbExecute(
      conn = src$con,
      statement = statement
    )

    return(result)
  }

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
    value <- jqr::jq(file(value), ".", flags = jqr::jq_flags(pretty = FALSE))
  } else if (isUrl(value)) {
    value <- jqr::jq(url(value), ".", flags = jqr::jq_flags(pretty = FALSE))
  }

  # handle potential json string input
  if (length(value) == 1 && is.atomic(value) &&
      is.character(value) && jsonlite::validate(value)) {
    # check format
    if (all(jqr::jq(value, " .[] | type ") == '"array"') && length(jqr::jq(value, " .[] ")) > 1L) stop(
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
      value <- jsonlite::toJSON(value, dataframe = "rows", auto_unbox = TRUE, digits = NA)
      value <- jqr::jq(value, " .[] ")
    } else {
      # otherwise keep as single document
      value <- jsonlite::toJSON(value, dataframe = "columns", auto_unbox = TRUE, digits = NA)
    }
  }

  # list to json
  if (all(class(value) %in% "list")) {
    value <- jsonlite::toJSON(value, auto_unbox = TRUE, digits = NA)
    # check if top level is an array
    chk <- jqr::jq(value, " type ")
    if (length(chk) == 1L && chk == '"array"') value <- jqr::jq(value, " .[] ")
  }

  # get doc ids to update
  # bulk update: if ids are in value, query is ignored
  ids <- try(jqr::jq(value, " ._id "), silent = TRUE)
  if (inherits(ids, "try-error")) ids <- NULL
  ids <- gsub("\"", "", as.character(ids))
  ids <- ids[ids != "null"]
  if (length(ids)) {
    if (query != "{}") warning(
      "Ignoring the specified 'query' parameter, using _id's ",
      "found in 'value' to identify documents to be updated")
    value <- jqr::jq(value, " del(._id) ")
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
    if (inherits(src, "src_duckdb")) {
      tmpRes <- try(
        DBI::dbExecute(
          conn = src$con,
          statement = statement
        ),
        silent = TRUE
      )
    } else {
      tmpRes <- try(
        DBI::dbWithTransaction(
          conn = src$con,
          code = {
            DBI::dbExecute(
              conn = src$con,
              statement = statement
            )
          }
        ),
        silent = TRUE
      )
    } # if

    if (!inherits(tmpRes, "try-error")) {
      result <- result + tmpRes
    } else {
      message(
        "Failed to update _id(s) ",
        paste0(ids[i], collapse = " / "),
        ", reason: ", tmpRes)
    }

  } # for

  # return
  return(result)

}
