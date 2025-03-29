#' Create documents in a database
#'
#' A message is emitted if the container `key` already exists.
#'
#' An error is raised for document(s) in `value` when their
#' `_id` already exist(s) in the collection `key`;
#' use [docdb_update()] to update such document(s).
#'
#' @section Identifiers:
#' If `value` is a data.frame that has a column `_id`,
#' or is a JSON string having a key `_id` at root level,
#' or is a list having an item `_id` at its top level,
#' this will be used as `_id`'s and primary index
#' in the database. If there are no such `_id`'s in `value`,
#' row names (if any exist) of `value` will be used as `_id`'s,
#' otherwise random `_id`'s will be created (using
#' [uuid::UUIDgenerate()] with \code{use.time = TRUE} for
#' SQLite and PostgreSQL, or using DuckDB's built-in `uuid()`).
#'
#' @param src Source object, result of call to any of functions
#' [src_mongo()], [src_sqlite()], [src_elastic()], [src_couchdb()]
#' [src_duckdb()] or [src_postgres()]
#'
#' @param key (character) The name of the container in the
#' database backend (corresponds to `collection` for MongoDB,
#' `dbname` for CouchDB, `index` for Elasticsearch, and to
#' a table name for DuckDB, SQLite and PostgreSQL)
#'
#' @param value The data to be created in the database:
#' a single data.frame, a JSON string, a list, or a
#' file name or URL that points to NDJSON documents
#'
#' @param ... Passed to functions [sofa::db_bulk_create()],
#' [elastic::docs_bulk()], and [mongolite::mongo()]$insert()
#'
#' @return (integer) Number of successfully created documents
#'
#' @export
#'
#' @examples \dontrun{
#' src <- src_sqlite()
#' docdb_create(src,
#'   key = "diamonds_small",
#'   value = as.data.frame(diamonds[1:3000L, ])
#' )
#' head(docdb_get(src, "diamonds_small"))
#' docdb_create(src, key = "contacts", value = contacts)
#' docdb_get(src, "contacts")[["friends"]]
#' }
#'
docdb_create <- function(src, key, value, ...) {
  assert(src, "docdb_src")
  assert(key, "character")
  assert(value, c("data.frame", "list", "character"))

  if (is.data.frame(value) && any(grepl("[.]", names(value)))) {
    message(
      "Names of columns in data frame have dots, ",
      "they will conflict with dot path notation.")
  }

  UseMethod("docdb_create", src)
}

#' @export
docdb_create.src_couchdb <- function(src, key, value, ...) {

  # create database if not yet existing
  result <- try(sofa::db_create(src$con, dbname = key, delifexists = FALSE), silent = TRUE)
  if (inherits(result, "try-error")) {
    if (grepl("already exists", result))
      existsMessage(key) else stop(result, call. = FALSE)}

  ## convert into target list
  value <- value2list(value)

  # early exit and avoid errors when
  # attempting to insert into elastic
  if (!length(value)) return(0L)

  # insert data but do not use docid because
  # this is not used by sofa. however, sofa
  # does use _id's that are in the doc
  result <- sofa::db_bulk_create(
    # using list as canonical input format
    cushion = src$con,
    dbname = key,
    doc = value,
    ...
  )

  # prepare return value
  result <- unlist(result)
  errors <- result[names(result) == "error"]
  oks <- result[names(result) == "ok"] == "TRUE"

  if (length(errors)) {
    warning("Could not create ", length(errors),
            " documents, reason: ", unique(errors),
            call. = FALSE, immediate. = TRUE)
  }

  # return
  return(length(oks))
}

#' @export
docdb_create.src_elastic <- function(src, key, value, ...) {

  # create database if not yet existing
  result <- try(elastic::index_create(src$con, index = key, verbose = FALSE), silent = TRUE)
  if (inherits(result, "try-error")) {
    if (grepl("already exists", result))
      existsMessage(key) else stop(result, call. = FALSE)}

  ## convert into target list
  value <- value2list(value)

  # extract docid's
  docids <- sapply(value, "[[", "_id", USE.NAMES = FALSE, simplify = TRUE)
  # remove _id's from list
  value <- lapply(value, function(i) {i[["_id"]] <- NULL; i})

  # early exit and avoid errors when
  # attempting to insert into elastic
  if (!length(value)) return(0L)

  # create using rownames as _id's
  result <- elastic::docs_bulk(
    conn = src$con,
    x = value,
    index = key,
    doc_ids = docids,
    es_ids = FALSE,
    quiet = TRUE,
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
  result <- result[names(result) == "items.index.result"]
  if (any(result != "created")) {
    warning("Could not create some documents, reason: ",
            unique(result[result != "created"]), call. = FALSE, immediate. = TRUE)
  }
  result <- sum(result == "created")
  return(result)

}


#' @export
docdb_create.src_mongo <- function(src, key, value, ...) {

  # special check for mongo
  chkSrcMongo(src, key)
  if (docdb_exists(src, key, value, ...)) existsMessage(key)

  # directly import ndjson file
  if ((all(class(value) %in% "character")) &&
      (length(value)  == 1L) &&
      (isUrl(value) || isFile(value))) {

    # turn into connection
    if (isFile(value)) {
      value <- file(value)
    } else if (isUrl(value)) {
      value <- url(value)
    }

    # import
    result <- try(
      suppressWarnings(
        src$con$import(
          # Stream import data in jsonlines format from a
          # connection, similar to the mongoimport utility.
          con = value
        )),
      silent = TRUE)

  } else {

    # since mongolite does not accept valid JSON strings
    # that come as an one-element array, convert to list
    if (inherits(value, "character")) {
      value <- jsonlite::fromJSON(value, simplifyVector = FALSE)
    }

    # mongolite uses _id columns in dataframes as object _id's:
    if (inherits(value, "data.frame")) {
      if (is.na(match("_id", names(value)))) {
        # if no _id column
        if (!identical(rownames(value),
                       as.character(seq_len(nrow(value))))) {
          # use rownames as _id's and remove rownames
          value[["_id"]] <- row.names(value)
          # row.names(value) <- NULL
        } else {
          # add canonical _id's
          value[["_id"]] <- uuid::UUIDgenerate(
            use.time = TRUE, n = nrow(value))
        }
      } # if no _id column
      # early return if no data
      if (!nrow(value)) return(0L)
      # convert to ndjson which is the format in which errors are raised
      # if names (keys) had problematic characters such as . or $
      value <- items2ndjson(value, mergeIdCol = TRUE)
    } # if data.frame

    # convert lists (incl. from previous step) to NDJSON
    if (inherits(value, "list")) {
      # add canonical _id's
      if (((!is.null(names(value)) &&
            !any(names(value) == "_id")) &&
           !any(sapply(value, function(i) any(names(i) == "_id")))) ||
          !any(names(unlist(value)) == "_id")
      ) {
        value <- lapply(value, function(i) c(
          "_id" = uuid::UUIDgenerate(use.time = TRUE), i))
      }
      # split list into vector of ndjson records
      value <- items2ndjson(value, mergeIdCol = TRUE)
    }

    # insert data.frame or JSON
    result <- try(
      suppressWarnings(
        src$con$insert(
          # loading either a dataframe
          # or vector of NDJSON strings
          data = value,
          auto_unbox = TRUE,
          digits = NA, ...))[["nInserted"]],
      silent = TRUE)

  } # if file access directly import ndjson file

  # generate user info
  if (inherits(result, "try-error") ||
      any(grepl("error", result))) {
    error <- result[grepl("rror", result)][[1]]
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
docdb_create.src_sqlite <- function(src, key, value, ...) {

  # reference: https://www.sqlite.org/json1.html

  # if table does not exist, create one
  if (!docdb_exists(src, key, ...)) {

    # standard for a nodbi json table in sqlite
    # standard: columns _id and json
    out <- try(
      DBI::dbWithTransaction(
        conn = src$con,
        code = {
          DBI::dbExecute(
            conn = src$con,
            statement = paste0(
              "CREATE TABLE \"", key, "\"",
              " ( _id TEXT PRIMARY_KEY NOT NULL,",
              "  json JSONB);"))
          DBI::dbExecute(
            conn = src$con,
            statement = paste0(
              "CREATE UNIQUE INDEX ",
              "\"", key, "_index\" ON ",
              "\"", key, "\" ( _id );"))
        }), silent = TRUE)

    if (inherits(out, "try-error") &&
        !grepl(" already exists", out)) stop(out)

  } else {
    existsMessage(key)
  }


  ## check features
  if (src$featUuid &&
      isFile(value)) {


    # turn value into ndjson file
    value <- normalizePath(value)

    # import into temporary table
    tblName <- uuid::UUIDgenerate()
    try(DBI::dbRemoveTable(src$con, tblName), silent = TRUE)
    on.exit(try(DBI::dbRemoveTable(src$con, tblName), silent = TRUE), add = TRUE)

    # for parameters see
    # https://github.com/r-dbi/RSQLite/blob/main/R/dbWriteTable_SQLiteConnection_character_character.R#L58
    RSQLite::dbWriteTable(
      conn = src$con,
      name = tblName,
      value = value,
      field.types = c("json" = "JSONB"),
      sep = "@@~~@||", # should not occur in input
      header = FALSE,
      skip = 0L,
      append = FALSE
    )

    # import from ndjson file
    result <- try(
      DBI::dbExecute(
        conn = src$con,
        statement = paste0(
          "INSERT INTO \"", key, "\"",
          " SELECT CASE WHEN length(json->>'$._id') > 0 THEN",
          " json->>'$._id' ELSE uuid() END AS _id,",
          " jsonb_patch(json, '{\"_id\": null}') AS json",
          " FROM '", tblName, "';")
      ), silent = TRUE)


  } else {

    # regular import using DBI::dbAppendTable
    result <- appendTable(src, key, value, ...)

  } # if file


  # prepare returns
  if (inherits(result, "try-error")) {
    error <- trimws(sub(".+: (.*?):.+", "\\1", result[[1]]))
    warning(
      "Could not create some documents, reason: ",
      unique(error), call. = FALSE, immediate. = TRUE)
    result <- 0L
  }
  return(sum(result))

}

#' @export
docdb_create.src_postgres <- function(src, key, value, ...) {

  # reference: https://www.postgresql.org/docs/current/datatype-json.html

  # if table does not exist, create one
  if (!docdb_exists(src, key, ...)) {

    # standard for a nodbi json table in PostgreSQL
    # standard: columns _id and json
    out <- try({
      # dbiWithTransaction does not work
      DBI::dbExecute(
        conn = src$con,
        statement = paste0(
          "CREATE TABLE \"", key, "\"",
          " ( _id TEXT PRIMARY KEY,",
          "  json JSONB);"))
      DBI::dbExecute(
        conn = src$con,
        statement = paste0(
          "CREATE UNIQUE INDEX ",
          "\"", key, "_index\" ON ",
          "\"", key, "\" ( _id );"))
    }, silent = TRUE)

    if (inherits(out, "try-error") &&
        !grepl(" already exists", out)) stop(out)

  } else {
    existsMessage(key)
  }

  ## localhost may be able to import from file,
  ## depending on postgres process rights
  copyWorked <- 0L
  if (isFile(value) &&
      grepl("^localhost$", src$host)) {

    value <- normalizePath(value)

    # import into temporary table
    tblName <- uuid::UUIDgenerate()
    try(DBI::dbRemoveTable(src$con, tblName), silent = TRUE)
    on.exit(try(DBI::dbRemoveTable(src$con, tblName), silent = TRUE), add = TRUE)

    # need to read with options to avoid the error
    # Character with value 0x0a must be escaped
    DBI::dbCreateTable(
      conn =  src$con,
      name = tblName,
      fields = c("json" = "JSONB")
    )

    copyWorked <- try(
      DBI::dbExecute(
        conn = src$con,
        statement = paste0(
          "COPY \"", tblName, '" ',
          "FROM '", value, "' ",
          "CSV QUOTE e'\x01' DELIMITER e'\x02';")
      ), silent = TRUE)

  }

  if (!inherits(copyWorked, "try-error") &&
      (copyWorked >= 1L)) {

    # import from ndjson file
    result <- try(
      DBI::dbExecute(
        conn = src$con,
        statement = paste0(
          "INSERT INTO \"", key, "\" SELECT",
          " CASE WHEN length(json->>'_id') > 0 THEN",
          " json->>'_id' ELSE gen_random_uuid()::TEXT END AS _id,",
          " CASE WHEN length(json->>'_id') > 0 THEN",
          " jsonb_patch(json, '{\"_id\": null}') ELSE json END AS json",
          " FROM \"", tblName, "\";")
      ), silent = TRUE)

  } else {

    # regular import using DBI::dbAppendTable
    result <- appendTable(src, key, value, ...)

  } # if localhost and file and copyWorked


  # prepare returns
  if (inherits(result, "try-error")) {
    error <- trimws(sub(".+ERROR: (.*?)[:\"].+", "\\1", result[[1]]))
    warning(
      "Could not create some documents, reason: ",
      unique(error), call. = FALSE, immediate. = TRUE)
    result <- 0L
  }
  return(sum(result))

}

#' @export
docdb_create.src_duckdb <- function(src, key, value, ...) {

  # if table does not exist, create one
  if (!docdb_exists(src, key, ...)) {

    # standard for a nodbi json table in duckdb
    # columns _id and json
    out <- try(
      DBI::dbWithTransaction(
        conn = src$con,
        code = {
          DBI::dbExecute(
            conn = src$con,
            statement = paste0(
              "CREATE TABLE \"", key, "\"",
              " ( _id TEXT PRIMARY KEY NOT NULL,",
              "  json JSON);"))
          DBI::dbExecute(
            conn = src$con,
            statement = paste0(
              "CREATE UNIQUE INDEX ",
              "\"", key, "_index\" ON ",
              "\"", key, "\" ( _id );"))
        }), silent = TRUE)

    if (inherits(out, "try-error") &&
        !grepl(" already exists", out)) stop(out)

  } else {
    existsMessage(key)
  }

  # https://duckdb.org/docs/api/r.html
  # Read-only mode is required if multiple R processes
  # want to access the same database file at the same time.

  # https://duckdb.org/docs/api/r.html#efficient-transfer
  # https://duckdb.org/docs/extensions/json#json-creation-functions

  if (isFile(value)) {

    value <- normalizePath(value)

    # import from ndjson file
    result <- try(
      DBI::dbExecute(
        conn = src$con,
        statement = paste0(
          "INSERT INTO \"", key, "\"",
          " SELECT CASE WHEN len(json->>'$._id') > 0 THEN",
          " json->>'$._id' ELSE format('{}', uuid()) END AS _id,",
          " json_merge_patch(json, '{\"_id\": null}') AS json ",
          " FROM read_ndjson_objects('",
          value, "');")
      ), silent = TRUE)


  } else {

    # regular import using DBI::dbAppendTable
    result <- appendTable(src, key, value, ...)

  } # if


  # prepare returns
  if (inherits(result, "try-error")) {
    error <- trimws(sub(".+: (.*?):.+", "\\1", result[[1]]))
    warning(
      "Could not create some documents, reason: ",
      unique(error), call. = FALSE, immediate. = TRUE)
    result <- 0L
  }
  return(result)

}


## helpers --------------------------------------

#' @keywords internal
#' @noRd
existsMessage <- function(k) {
  message(paste0("Note: container '", k, "' already exists"))
}

#' @keywords internal
#' @noRd
isUrl <- function(x) {

  # check if x is may be an url
  if (length(x) != 1L ||
      !is.atomic(x) ||
      !inherits(x, "character")) return(FALSE)

  return(grepl("^https?://", x))
}

#' @keywords internal
#' @noRd
isFile <- function(x) {

  # check if x is the name of a readable file

  if (length(x) != 1L ||
      !is.atomic(x) ||
      !inherits(x, "character")) return(FALSE)

  if (!file.exists(x)) return(FALSE)

  out <- try(
    suppressWarnings(
      file.access(x, mode = 4)[[1]]) == 0L,
    silent = TRUE)

  if (inherits(out, "try-error") || !out) return(FALSE)

  return(file.size(x) > 5L)

}

#' @keywords internal
#' @noRd
items2ndjson <- function(df, mergeIdCol = FALSE) {

  # - function takes a data frame or list
  # - if mergeIdCol, builds ndjson vector from df items
  # - if not, builds ndjson from columns except _id
  #   returns data frame with columns _id and json

  if (!inherits(df, "data.frame") && !inherits(df, "list")) stop()

  if (inherits(df, "data.frame")) row.names(df) <- NULL

  if (inherits(df, "list")) df <-
      jsonlite::fromJSON(
        jsonlite::toJSON(df, auto_unbox = TRUE, digits = NA))

  # temporary file and connection
  tfname <- tempfile()
  on.exit(try(unlink(tfname), silent = TRUE), add = TRUE)

  tfnameCon <- file(tfname, open = "wt")
  on.exit(try(close(tfnameCon), silent = TRUE), add = TRUE)

  if (mergeIdCol) {

    jsonlite::stream_out(
      x = df,
      con = tfnameCon,
      verbose = FALSE,
      auto_unbox = TRUE,
      pagesize = 5000L,
      digits = NA)

    close(tfnameCon)
    tfnameCon <- file(tfname, open = "rt")
    on.exit(try(close(tfnameCon), silent = TRUE), add = TRUE)

    return(readLines(tfnameCon))

  } else {

    jsonlite::stream_out(
      x = df[, -match("_id", names(df)), drop = FALSE],
      con = tfnameCon,
      verbose = FALSE,
      auto_unbox = TRUE,
      pagesize = 5000L,
      digits = NA)

    close(tfnameCon)
    tfnameCon <- file(tfname, open = "rt")
    on.exit(try(close(tfnameCon), silent = TRUE), add = TRUE)

    return(
      data.frame(
        "_id" = df[["_id"]],
        "json" = readLines(tfnameCon),
        check.names = FALSE
      ))
  }
}

#' @keywords internal
#' @noRd
appendTable <- function(src, key, value, ...) {

  ## regular import using DBI::dbAppendTable

  # convert lists to json
  if (inherits(value, "list")) {
    value <- jsonlite::toJSON(value, auto_unbox = TRUE, digits = NA)
  }

  # convert JSON string to data frame
  if (inherits(value, c("character", "json"))) {
    # target is data frame for next section

    # convert ndjson file or json string to data frame
    if (all(class(value) %in% "character") &&
        (length(value)  == 1L) &&
        (isUrl(value) || isFile(value))) {

      if (isFile(value)) {
        value <- jsonlite::stream_in(con = file(value), verbose = FALSE)
      } else if (isUrl(value)) {
        value <- jsonlite::stream_in(con = url(value), verbose = FALSE)
      }

    } else {
      value <- jsonlite::fromJSON(value)
    }

  }

  # data frame
  if (inherits(value, "data.frame")) {
    #
    if (is.na(match("_id", names(value)))) {
      # if no _id column
      if (!identical(
        rownames(value),
        as.character(seq_len(nrow(value))))) {
        # use rownames as _id's and remove rownames
        value[["_id"]] <- row.names(value)
      } else {
        # add canonical _id's
        value[["_id"]] <- uuid::UUIDgenerate(
          use.time = TRUE, n = nrow(value))
      }
    } # if no _id column
    #
    if (ncol(value) > 2L ||
        (ncol(value) == 2L &&
         !all(sort(names(value)) == c("_id", "json")))) {
      # no json column; convert to df with ndjson in json column
      value <- items2ndjson(value)
    }
    #
    if (all(class(value[["_id"]]) %in% "list")) {
      # in case fromJSON created the _id column as list
      value[["_id"]] <- unlist(value[["_id"]])
    }
  } # if data.frame

  # insert data
  return(try(
    DBI::dbAppendTable(
      conn = src$con,
      name = key,
      # canonical 'value': a data frame
      # with 2 columns, _id and json
      value = value,
      ...),
    silent = TRUE))

}

#' @keywords internal
#' @noRd
value2list <- function(value) {

  # convert data frame to list
  if (inherits(value, "data.frame")) {
    if (is.na(match("_id", names(value)))) {
      # if no _id column
      if (!identical(rownames(value),
                     as.character(seq_len(nrow(value))))) {
        # use rownames as _id's and remove rownames
        value[["_id"]] <- row.names(value)
        row.names(value) <- NULL
      }
    }
    row.names(value) <- NULL
    value <- jsonlite::fromJSON(
      jsonlite::toJSON(value, auto_unbox = TRUE, digits = NA),
      simplifyVector = FALSE)
  } # if data.frame

  # convert JSON string to list
  if (inherits(value, "character")) {
    if (all(class(value) %in% "character") && (length(value)  == 1L) &&
        (isUrl(value) || isFile(value))) {
      # read ndjson file or url
      if (isFile(value)) {
        value <- jsonlite::stream_in(con = file(value), simplifyVector = FALSE, verbose = FALSE)
      } else if (isUrl(value)) {
        value <- jsonlite::stream_in(con = url(value), simplifyVector = FALSE, verbose = FALSE)
      }
    } else {
      # read json string
      value <- jsonlite::fromJSON(value, simplifyVector = FALSE)
    }
  }

  # mangle lists
  if (inherits(value, "list")) {
    if (all(sapply(lapply(value, "[[", "_id"), is.null))) {
      # add canonical _id's
      value <- lapply(value, function(i) c(
        "_id" = uuid::UUIDgenerate(use.time = TRUE), i))
    }
  }

  # return
  return(value)
}


