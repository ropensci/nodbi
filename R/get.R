#' Get all documents from container in database
#'
#' @inheritParams docdb_create
#'
#' @param limit (integer) Maximum number of documents
#'  to return. If `NULL` or not set (default), 10,000 for
#'  Elasticsearch and all documents for MongoDB, SQLite,
#'  CouchDB, PostgreSQL, and DuckDB.
#'
#' @param ... Passed on to functions:
#' - MongoDB: find() in [mongolite::mongo()]
#' - SQLite: ignored
#' - Elasticsearch: [elastic::Search()]
#' - CouchDB: [sofa::db_alldocs()]
#' - PostgreSQL: ignored
#' - DuckDB: ignored
#'
#' @return A data frame, one document per row
#'
#' @export
#'
#' @examples \dontrun{
#' src <- src_sqlite()
#' docdb_create(src, "mtcars", mtcars)
#' docdb_get(src, "mtcars", limit = 10L)
#' }
docdb_get <- function(src, key, limit = NULL, ...) {

  assert(src, "docdb_src")
  assert(key, "character")
  assert(limit, "integer")

  params <- list(...)
  if (length(params[["fields"]]) ||
      length(params[["query"]]) ||
      length(params[["listfields"]])) stop(
    "Use docdb_query() to specify fields or query parameters.")

  # dispatch
  UseMethod("docdb_get", src)
}

#' @export
docdb_get.src_couchdb <- function(src, key, limit = NULL, ...) {

  jsonlite::fromJSON(
    # get data
    sofa::db_alldocs(
      cushion = src$con,
      dbname = key,
      as = "json",
      include_docs = TRUE,
      # sorting may not work
      descending = FALSE,
      limit = limit
      # keep only data
    ))[["rows"]][["doc"]][
      # remove _rev column
      , -2, drop = FALSE]

}

#' @export
docdb_get.src_elastic <- function(src, key, limit = NULL, ...) {

  # adjust parameter
  if (is.null(limit)) limit <- 10000L

  # get all _id's
  docids <- elastic::Search(
    conn = src$con, index = key, source = FALSE,
    size = limit, ...)[["hits"]][["hits"]]

  # process id's
  if (!length(docids)) return(NULL)
  docids <- sapply(docids, "[[", "_id", USE.NAMES = FALSE, simplify = TRUE)
  docids <- sort(docids)
  if (is.null(docids)) return(NULL)

  # get results
  if (length(docids) == 1L) {
    result <- elastic::docs_get(
      conn = src$con, index = key, id = docids, raw = FALSE,
      verbose = FALSE, ...)
    result <- c("_id" = docids, result[["_source"]])
    result <- jsonlite::fromJSON(jsonlite::toJSON(result, auto_unbox = TRUE, digits = NA))
    result <- data.frame(t(result), stringsAsFactors = FALSE, check.names = FALSE)
  } else {
    result <- elastic::docs_mget(
      src$con, index = key, ids = docids,
      verbose = FALSE, ...)[["docs"]]
    result <- lapply(result, function(i) c("_id" = i[["_id"]], i[["_source"]]))
    result <- jsonlite::fromJSON(jsonlite::toJSON(result, auto_unbox = TRUE, digits = NA))
  }

  # output
  return(result)

}

#' @export
docdb_get.src_mongo <- function(src, key, limit = NULL, ...) {

  # check congruency
  chkSrcMongo(src, key)

  # check params and use limit
  params <- list(...)
  if (is.null(limit)) limit <- 0L

  # remove rownames
  return(`rownames<-`(
    # get data
    src$con$find(
      limit = limit,
      fields = '{}',
      # canonical sorting in nodbi
      sort = '{"_id": 1}'),
    NULL))

}

#' @export
docdb_get.src_sqlite <- function(src, key, limit = NULL, ...) {

  getFunction <- "json"
  return(sqlGet(src = src, key = key, limit = limit, getFunction = getFunction, ...))
}

#' @export
docdb_get.src_postgres <- function(src, key, limit = NULL, ...) {

  getFunction <- "json::TEXT"
  return(sqlGet(src = src, key = key, limit = limit, getFunction = getFunction, ...))
}

#' @export
docdb_get.src_duckdb <- function(src, key, limit = NULL, ...) {

  getFunction <- "json"
  return(sqlGet(src = src, key = key, limit = limit, getFunction = getFunction, ...))
}

## helpers --------------------------------------

#' @keywords internal
#' @noRd
sqlGet <- function(src, key, limit = NULL, getFunction, ...) {

  # set limit if not null
  n <- -1L
  if (!is.null(limit)) n <- limit

  # compose query statment
  statement <- paste0(
    "SELECT '{\"_id\": \"' || _id || '\", ' || LTRIM(", getFunction, ", '{') ",
    "AS json FROM \"", key, "\" WHERE json != '{}' ",
    # canonical sorting in nodbi
    "ORDER BY _id ASC;")

  # temporary file for streaming
  tfname <- tempfile()
  tfnameCon <- file(description = tfname, open = "wt")
  # register to remove file after used for streaming
  on.exit(try(close(tfnameCon), silent = TRUE), add = TRUE)
  on.exit(try(unlink(tfname), silent = TRUE), add = TRUE)

  # get data, write to file in ndjson format
  writeLines(
    stringi::stri_replace_all_fixed(
      str = paste0(
        "", # protect against empty query result
        stats::na.omit(  # eliminate rows without json
          DBI::dbGetQuery(
            conn = src$con,
            statement = statement,
            n = n)[["json"]])),
      pattern = "\n",
      replacement = "\\n"),
    con = tfnameCon,
    sep = "\n",
    useBytes = TRUE)
  close(tfnameCon)

  # stream in ndjson records
  return(jsonlite::stream_in(file(tfname), verbose = FALSE))

}
