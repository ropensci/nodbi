#' Update documents
#'
#' Documents identified by the query are updated
#' by patching their JSON with \code{value}.
#' This is native with MongoDB and SQLite and is
#' emulated for Elasticsearch and CouchDB using
#' SQLite/JSON1, and uses a plpgsql function for
#' PostgreSQL.
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

  # therefore emulate update for couchdb as follows
  if (!length(find.package("RSQLite", quiet = TRUE))) {
    stop("Package RSQLite needed to calculate json update, ",
         "but it is not installed. Install or use docdb_create().",
         call. = FALSE)
  }

  # get original set
  input <- docdb_query(src, key, query)

  # early return if not found
  if (!length(input)) return(0L)

  # data frame to json
  if (class(value) == "data.frame") {
    value <- jsonify::to_json(value, by = "col", unbox = TRUE)
  }
  # list to json
  if (class(value) == "list") {
    value <- jsonify::to_json(value, unbox = TRUE)
  }

  # calculate new document(s)
  value <- jsonify::from_ndjson(
    paste0(
      sapply(
        strsplit(
          jsonify::to_ndjson(input),
          split = "\n")[[1]],
        function(i)
          jsonUpdate(
            jsonT = i,
            jsonP = value),
        USE.NAMES = FALSE,
        simplify = TRUE),
      collapse = "\n"),
    simplify = TRUE)

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
  ids <- docdb_query(src, key, query, fields = '{"_id": 1}')[[1]]

  # early return if not found
  if (!length(ids)) return(0L)

  # json to data frame
  if (class(value) == "character") {
    value <- jsonlite::fromJSON(value)
  }

  # list to data frame
  if (class(value) == "list") {
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

  # Error: no 'docs_bulk_update' method for class list
  # therefore using data frame only for update
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

  # data frame to json
  if (class(value) == "data.frame") {
    value <- jsonify::to_json(value, by = "col", unbox = TRUE)
  }
  # list to json
  if (class(value) == "list") {
    value <- jsonify::to_json(value, unbox = TRUE)
  }

  # turn into json set
  value <- paste0('{"$set":', value, "}")

  # do update
  result <- try(
    suppressWarnings(
      src$con$update(
        query = query,
        update = value,
        upsert = FALSE,
        multiple = TRUE,
        ...))[c("matchedCount", "upsertedCount")],
    silent = TRUE)
  result <- unlist(result)

  # generate user info
  if (inherits(result, "try-error") ||
      any(grepl("error", result))) {
    error <- result[grepl("rror", result)]
    error <- trimws(sub(".+E[0-9]+(.*?):.+", "\\1", error))
    warning(
      "Could not create some documents, reason: ",
      unique(error), call. = FALSE, immediate. = TRUE)
    result <- result[!grepl("rror", result)]
    result <- min(0, as.integer(result))
  }
  return(max(result))

}

#' @export
docdb_update.src_sqlite <- function(src, key, value, query, ...) {

  # data frame to json
  if (class(value) == "data.frame") {
    value <- jsonify::to_json(value, by = "col", unbox = TRUE)
  }
  # list to json
  if (class(value) == "list") {
    value <- jsonify::to_json(value, unbox = TRUE)
  }

  # get docs to update
  ids <- docdb_query(src, key, query, fields = '{"_id": 1}')[["_id"]]

  # SQL for patching, see https://www.sqlite.org/json1.html#jpatch
  statement <- paste0(
    'UPDATE "', key, '" SET json = json_patch(json,\'',
    value, '\') WHERE _id IN (',
    paste0('"', ids, '"', collapse = ","), ');'
  )

  # update data
  result <- try(
    dbWithTransaction(
      src$con, {
        DBI::dbExecute(
          conn = src$con,
          statement = statement
        )
      }),
    silent = TRUE)

  # return
  return(result)

}

#' @export
docdb_update.src_postgres <- function(src, key, value, query, ...) {

  # data frame to json
  if (class(value) == "data.frame") {
    value <- jsonify::to_json(value, by = "col", unbox = TRUE)
  }
  # list to json
  if (class(value) == "list") {
    value <- jsonify::to_json(value, unbox = TRUE)
  }

  # get docs to update
  ids <- docdb_query(src, key, query, fields = '{"_id": 1}')[["_id"]]

  # Since PostgreSQL has no internal function,
  # uses a function inserted by src_postgres
  statement <- paste0(
    'UPDATE "', key, '" SET json = jsonb_merge_patch(json,\'',
    value, "') WHERE _id IN (",
    paste0("'", ids, "'", collapse = ","), ");"
  )

  # update data
  result <- try(
    DBI::dbWithTransaction(
      src$con, {
        DBI::dbExecute(
          conn = src$con,
          statement = statement
        )
      }),
    silent = TRUE)

  # return
  return(result)

}


## helpers --------------------------------------

# emulating json update by calculating
# the patch result using RSQLite
jsonUpdate <- function(jsonT, jsonP) {
  # https://www.sqlite.org/json1.html#jpatch

  # get connection
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")

  # register deconstructor
  on.exit(try(DBI::dbDisconnect(con), silent = TRUE), add = TRUE)

  # construct json command
  jsonPatch <- paste0(
    "SELECT json_patch('",
    jsonT, "','", jsonP,
    "') AS json;"
  )

  # calculate patch
  out <- DBI::dbGetQuery(
    conn = con,
    statement = jsonPatch)

  # return
  return(out[[1]])

}
