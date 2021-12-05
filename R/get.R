#' Get all documents from container in database
#'
#' @inheritParams docdb_create
#'
#' @param limit (integer) Maximum number of documents
#'  to return (defaults to all for MongoDB,
#'  all for SQLite, 10,000 for Elasticsearch,
#'  all for CouchDB, and all for PostgreSQL
#'
#' @param ... Passed on to functions:
#' - MongoDB: find() in [mongolite::mongo()]
#' - SQLite: ignored
#' - Elasticsearch: [elastic::Search()]
#' - CouchDB: [sofa::db_alldocs()]
#' - PostgreSQL: ignored
#'
#' @return Document(s) in a data frame
#'
#' @export
#'
#' @examples \dontrun{
#' src <- src_sqlite()
#' docdb_create(src, "mtcars", mtcars)
#' docdb_get(src, "mtcars", limit = 10L)
#' }
docdb_get <- function(src, key, limit = NULL, ...) {
  params <- list(...)
  if (length(params[["fields"]]) | length(params[["query"]])) stop(
    "Use docdb_query() to specify fields or query parameters.")
  #
  assert(src, "docdb_src")
  assert(key, "character")
  assert(limit, "integer")
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
      descending = FALSE
      # keep only data
    ))[["rows"]][["doc"]][
      # remove _rev column
      , -2, drop = FALSE]

}

#' @export
docdb_get.src_elastic <- function(src, key, limit = "10000", ...) {

  # get all _id's
  docids <- elastic::Search(
    src$con, key, source = FALSE,
    size = limit, ...)[["hits"]][["hits"]]
  docids <- sapply(docids, "[[", "_id", USE.NAMES = FALSE, simplify = TRUE)
  docids <- sort(docids)

  # check for empty index
  if (is.null(docids)) return(NULL)

  # get results
  if (length(docids) == 1L) {
    result <- elastic::docs_get(
      src$con, index = key, id = docids, raw = FALSE,
      verbose = FALSE, ...)
    result <- c("_id" = docids, result[["_source"]])
    result <- jsonlite::fromJSON(jsonlite::toJSON(result, auto_unbox = TRUE))
    result <- data.frame(t(result), stringsAsFactors = FALSE, check.names = FALSE)
  } else {
    result <- elastic::docs_mget(
      src$con, index = key, ids = docids,
      verbose = FALSE, ...)[["docs"]]
    result <- lapply(result, function(i) c("_id" = i[["_id"]], i[["_source"]]))
    result <- jsonlite::fromJSON(jsonlite::toJSON(result, auto_unbox = TRUE))
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
  if (length(params[["sort"]])) params[["sort"]] <- NULL
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

  statement <- paste0(
    "SELECT '{\"_id\": \"' || _id || '\", ' || LTRIM(json, '{') ",
    "AS json FROM \"", key, "\" ",
    # canonical sorting in nodbi
    "ORDER BY _id ASC;")

  # set limit if not null
  n <- -1L
  if (!is.null(limit)) n <- limit

  # temporary file for streaming
  tfname <- tempfile()
  tfnameCon <- file(description = tfname, open = "wt", encoding = "native.enc")
  # register to remove file after used for streaming
  on.exit(try(close(tfnameCon), silent = TRUE), add = TRUE)
  on.exit(unlink(tfname), add = TRUE)

  # get data, write to file in ndjson format
  writeLines(
    # eliminate rows without any json
    stats::na.omit(
      DBI::dbGetQuery(
        conn = src$con,
        statement = statement,
        n = n)[["json"]]),
    con = tfnameCon,
    sep = "\n",
    useBytes = TRUE)
  close(tfnameCon)

  # stream in ndjson records
  return(jsonlite::stream_in(file(tfname, encoding = "UTF-8"), verbose = FALSE))

}

#' @export
docdb_get.src_postgres <- function(src, key, limit = NULL, ...) {

  # query to return full json column as text
  statement <- paste0(
    "SELECT '{\"_id\": \"' || _id || '\", ' || LTRIM(json::TEXT, '{') ",
    "AS json FROM \"", key, "\" ",
    # canonical sorting in nodbi
    "ORDER BY _id ASC;")

  # set limit if not null
  n <- -1L
  if (!is.null(limit)) n <- limit

  # temporary file for streaming
  tfname <- tempfile()
  tfnameCon <- file(description = tfname, open = "wt", encoding = "native.enc")
  # register to remove file after used for streaming
  on.exit(try(close(tfnameCon), silent = TRUE), add = TRUE)
  on.exit(unlink(tfname), add = TRUE)

  # get data, write to file in ndjson format
  writeLines(
    # eliminate rows without any json
    stats::na.omit(
      DBI::dbGetQuery(
        conn = src$con,
        statement = statement,
        n = n)[["json"]]),
    con = tfnameCon,
    sep = "\n",
    useBytes = TRUE)
  close(tfnameCon)

  # stream in ndjson records
  return(jsonlite::stream_in(file(tfname, encoding = "UTF-8"), verbose = FALSE))

}
