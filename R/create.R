#' Create documents in a database
#'
#' A message is emitted if the container `key` already exists.
#'
#' @section Identifiers:
#' Any _id's in `value` will be used as _id's and primary index
#' in the database. If there are no _id's in `value`,
#' row names (if any exist) will be used as _id's,
#' or random _id's will be created (using
#' [uuid::UUIDgenerate()] with \code{use.time = TRUE}).
#'
#' A warning is emitted if a document(s) with _id's already
#' exist in `value` and that document in `value` is not newly
#' created in the database; use [docdb_update()] to update
#' such document(s).
#'
#' @param src Source object, result of call to any of functions
#' [src_mongo()], [src_sqlite()], [src_elastic()], [src_couchdb()]
#' or [src_postgres()]
#'
#' @param key (character) A key as name of the container
#' (corresponds to parameter `collection` for MongoDB,
#' `dbname` for CouchDB, `index` for Elasticsearch and to
#' a table name for SQLite and for PostgreSQL)
#'
#' @param value The data to be created in the database:
#' a single data.frame, a JSON string or a list;
#' or the file name or URL of NDJSON documents
#'
#' @param ... Passed to functions:
#' - CouchDB: [sofa::db_bulk_create()]
#' - Elasticsearch: [elastic::docs_bulk]
#' - MongoDB: [mongolite::mongo()]
#' - SQLite: ignored
#' - PostgreSQL: ignored
#'
#' @return (integer) Number of successfully created documents
#'
#' @export
#'
#' @examples \dontrun{
#' src <- src_sqlite()
#' docdb_create(src, key = "diamonds_small",
#'   value = as.data.frame(diamonds[1:3000L,]))
#' head(docdb_get(src, "diamonds_small"))
#' docdb_create(src, key = "contacts", value = contacts)
#' docdb_get(src, "contacts")[["friends"]]
#' }
#'
docdb_create <- function(src, key, value, ...) {
  assert(src, "docdb_src")
  assert(key, "character")
  assert(value, c("data.frame", "list", "character"))
  UseMethod("docdb_create", src)
}

#' @export
docdb_create.src_couchdb <- function(src, key, value, ...) {

  # create database if not yet existing
  result <- try(sofa::db_create(src$con, dbname = key, delifexists = FALSE), silent = TRUE)
  if (inherits(result, "try-error") && grepl("already exists", result)) existsMessage(key)

  # convert into target list

  # data frame
  if (class(value) == "data.frame") {
    if (is.na(match("_id", names(value)))) {
      # if no _id column
      if (!identical(rownames(value),
                     as.character(seq_len(nrow(value))))) {
        # use rownames as _id's and remove rownames
        value[["_id"]] <- row.names(value)
        row.names(value) <- NULL
      }
    }
    value <- jsonify::from_json(jsonify::to_json(value, unbox = TRUE), simplify = FALSE)
  } # if data.frame

  # convert JSON string to list
  if (class(value) == "character") {
    if ((length(value)  == 1L) &&
        (isUrl(value) || isFile(value))) {
      # read ndjson file or url (does not work with jsonify)
      if (isFile(value)) {
        value <- jsonlite::stream_in(con = file(value), simplifyVector = FALSE, verbose = FALSE)
      } else if (isUrl(value)) {
        value <- jsonlite::stream_in(con = url(value), simplifyVector = FALSE, verbose = FALSE)
      }
    } else {
      # read json string
      value <- jsonify::from_json(value, simplify = FALSE)
    }
  }

  # mangle lists
  if (class(value) == "list") {
    if (all(sapply(lapply(value, "[[", "_id"), is.null))) {
      # add canonical _id's
      value <- lapply(value, function(i) c(
        "_id" = uuid::UUIDgenerate(use.time = TRUE), i))
    }
  }

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

  result <- try(elastic::index_create(src$con, index = key, verbose = FALSE), silent = TRUE)
  if (inherits(result, "try-error") && grepl("already exists", result)) existsMessage(key)

  # Loading the fielddata on the _id field is deprecated and will be removed in future versions.
  # If you require sorting or aggregating on this field you should also include the id in the
  # body of your documents, and map this field as a keyword field that has [doc_values] enabled

  # data frame
  if (class(value) == "data.frame") {
    if (is.na(match("_id", names(value)))) {
      # if no _id column
      if (!identical(rownames(value),
                     as.character(seq_len(nrow(value))))) {
        # use rownames as _id's and remove rownames
        docids <- row.names(value)
        row.names(value) <- NULL
      } else {
        # add canonical _id's
        docids <- uuid::UUIDgenerate(use.time = TRUE, n = nrow(value))
      }
    } else {
      # use _id column as _id's and remove it
      docids <- value[["_id"]]
      value <- value[, -match("_id", names(value))]
    } # _id column
  } # if data.frame

  # convert JSON string to list
  if (class(value) == "character") {

    # since cocument IDs cannot be passed in
    # with files, create list from file

    # convert ndjson file or json string to list
    if ((length(value)  == 1L) &&
        (isUrl(value) || isFile(value))) {
      if (isFile(value)) {
        value <- jsonlite::stream_in(con = file(value), simplifyVector = FALSE, verbose = FALSE)
      } else if (isUrl(value)) {
        value <- jsonlite::stream_in(con = url(value), simplifyVector = FALSE, verbose = FALSE)
      }
    } else {
      value <- jsonify::from_json(value, simplify = FALSE)
    }
  }

  # mangle lists
  if (class(value) == "list") {
    if (all(sapply(lapply(value, "[[", "_id"), is.null))) {
      # add canonical _id's
      docids <- uuid::UUIDgenerate(use.time = TRUE, n = length(value))
    } else {
      docids <- sapply(value, "[[", "_id", USE.NAMES = FALSE, simplify = TRUE)
      # remove _id's from list
      value <- lapply(value, function(i) {i[["_id"]] <- NULL; i})
    }
  }

  # create using rownames as _id's
  # note: uses doc_as_upsert, which
  # is set to TRUE for all records
  result <- elastic::docs_bulk(
    conn = src$con,
    x = value,
    index = key,
    doc_ids = docids,
    es_ids = FALSE,
    quiet = TRUE,
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
  if ((class(value) == "character") &&
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
    if (class(value) == "character") {
      value <- jsonify::from_json(value, simplify = FALSE)
    }

    # mongolite uses _id columns in dataframes as object _id's:
    if (class(value) == "data.frame") {
      if (is.na(match("_id", names(value)))) {
        # if no _id column
        if (!identical(rownames(value),
                       as.character(seq_len(nrow(value))))) {
          # use rownames as _id's and remove rownames
          value[["_id"]] <- row.names(value)
          row.names(value) <- NULL
        } else {
          # add canonical _id's
          value[["_id"]] <- uuid::UUIDgenerate(use.time = TRUE, n = nrow(value))
        }
      } # if no _id column
      # early return if no data
      if (!nrow(value)) return(0L)
      # convert to ndjson which is the format in which errors are raised
      # if names (keys) had problematic characters such as . or $
      value <- strsplit(jsonify::to_ndjson(value, unbox = TRUE), split = "\n")[[1]]
    } # if data.frame

    # convert lists (incl. from previous step) to NDJSON
    if (class(value) == "list") {
      # add canonical _id's
      if ((!is.null(names(value)) && !any(names(value) == "_id")) &&
          !any(sapply(value, function(i) any(names(i) == "_id")))
      ) {
        value <- lapply(value, function(i) c(
          "_id" = uuid::UUIDgenerate(use.time = TRUE), i))
      }
      # split into vector of ndjson records
      value <- jsonify::to_ndjson(value, unbox = TRUE)
      if (any(grepl("\\}\n\\{", value))) {
        value <- strsplit(value, split = "[}]\n[{]")[[1]]
        value <- sub("^([^{])", "{\\1", sub("([^}])$", "\\1}", value))
      }
    }

    # insert data.frame or JSON
    result <- try(
      suppressWarnings(
        src$con$insert(
          # loading either a dataframe or
          # vector of NDJSON (!) strings
          data = value,
          auto_unbox = TRUE,
          digits = NA, ...))[["nInserted"]],
      silent = TRUE)

  } # if file access directly import ndjson file

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
  return(sum(result))

}

#' @export
docdb_create.src_sqlite <- function(src, key, value, ...) {

  # reference: https://www.sqlite.org/json1.html

  # if table does not exist, create one
  if (!docdb_exists(src, key, ...)) {

    # standard for a nodbi json table in sqlite
    # standard: columns _id and json
    dbWithTransaction(
      src$con, {
        DBI::dbExecute(
          conn = src$con,
          statement = paste0("CREATE TABLE \"", key, "\"",
                             " ( _id TEXT PRIMARY_KEY NOT NULL,",
                             "  json JSON);"))
        DBI::dbExecute(
          conn = src$con,
          statement = paste0("CREATE UNIQUE INDEX ",
                             "\"", key, "_index\" ON ",
                             "\"", key, "\" ( _id );"))
      })
  } else {
    existsMessage(key)
  }

  # convert lists to json
  if (class(value) == "list") {
    value <- jsonlite::toJSON(value, auto_unbox = TRUE, digits = NA)
  }

  # convert JSON string to data frame
  if (class(value) == "character" ||
      class(value) == "json") {
    # target is data frame for next section

    # convert ndjson file or json string to data frame
    if ((length(value)  == 1L) &&
        (isUrl(value) || isFile(value))) {
      if (isFile(value)) {
        value <- jsonlite::stream_in(con = file(value), verbose = FALSE)
      } else if (isUrl(value)) {
        value <- jsonlite::stream_in(con = url(value), verbose = FALSE)
      }
    } else {
      value <- jsonlite::fromJSON(value)
    }

    # process if value remained a list
    if (class(value) == "list") {

      # any _id (would be in top level of list)
      ids <- value[["_id"]]
      # remove _id's
      if (length(ids)) value[["_id"]] <- NULL
      # change back to json
      value <- as.character(
        jsonlite::toJSON(
          value,
          auto_unbox = TRUE,
          digits = NA))
      # construct data frame
      value <- data.frame(
        "_id" = ids,
        "json" = value,
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
    }
  }

  # data frame
  if (class(value) == "data.frame") {
    #
    if (is.na(match("_id", names(value)))) {
      # if no _id column
      if (!identical(rownames(value),
                     as.character(seq_len(nrow(value))))) {
        # use rownames as _id's and remove rownames
        value[["_id"]] <- row.names(value)
        row.names(value) <- NULL
      } else {
        # add canonical _id's
        value[["_id"]] <- uuid::UUIDgenerate(use.time = TRUE, n = nrow(value))
      }
    } # if no _id column
    #
    if (ncol(value) > 2L || (ncol(value) == 2L &&
                             !all(sort(names(value)) == c("_id", "json")))) {
      # convert if there is no json column yet
      value[["json"]] <- strsplit(
        jsonify::to_ndjson(
          value[, -match("_id", names(value)), drop = FALSE],
          unbox = TRUE),
        split = "\n"
      )[[1]]
      # remove original columns
      value <- value[, c("_id", "json"), drop = FALSE]
    }
    #
    if (class(value[["_id"]]) == "list") {
      # in case fromJSON created the _id column as list
      value[["_id"]] <- unlist(value[["_id"]])
    }
  } # if data.frame

  # insert data
  result <- try(
    dbWithTransaction(
      src$con, {
        DBI::dbAppendTable(
          conn = src$con,
          name = key,
          # canonical value: a data frame
          # with 2 columns, _id and json
          value = value)
      }),
    silent = TRUE)

  # prepare returns
  if (inherits(result, "try-error")) {
    error <- trimws(sub(".+: (.*?):.+", "\\1", result))
    warning(
      "Could not create some documents, reeason: ",
      unique(error), call. = FALSE, immediate. = TRUE)
    result <- 0L
  }
  return(sum(result))

}

#' @export
docdb_create.src_postgres <- function(src, key, value, ...) {

  # reference: https://www.sqlite.org/json1.html

  # if table does not exist, create one
  if (!docdb_exists(src, key, ...)) {

    # standard for a nodbi json table in PostgreSQL
    # standard: columns _id and json
    DBI::dbWithTransaction(
      src$con, {
        DBI::dbExecute(
          conn = src$con,
          statement = paste0("CREATE TABLE \"", key, "\"",
                             " ( _id TEXT PRIMARY KEY,",
                             "  json JSONB);"))

        DBI::dbExecute(
          conn = src$con,
          statement = paste0("CREATE UNIQUE INDEX ",
                             "\"", key, "_index\" ON ",
                             "\"", key, "\" ( _id );"))
      })
  } else {
    existsMessage(key)
  }

  # convert lists to json
  if (class(value) == "list") {
    value <- jsonlite::toJSON(value, auto_unbox = TRUE, digits = NA)
  }

  # convert JSON string to data frame
  if (class(value) == "character" ||
      class(value) == "json") {
    # target is data frame for next section

    # convert ndjson file or json string to data frame
    if ((length(value)  == 1L) &&
        (isUrl(value) || isFile(value))) {
      if (isFile(value)) {
        value <- jsonlite::stream_in(con = file(value), verbose = FALSE)
      } else if (isUrl(value)) {
        value <- jsonlite::stream_in(con = url(value), verbose = FALSE)
      }
    } else {
      value <- jsonlite::fromJSON(value)
    }

    # process if value remained a list
    if (class(value) == "list") {

      # any _id (would be in top level of list)
      ids <- value[["_id"]]
      # remove _id's
      if (length(ids)) value[["_id"]] <- NULL
      # change back to json
      value <- as.character(
        jsonlite::toJSON(
          value,
          auto_unbox = TRUE,
          digits = NA))
      # construct data frame
      value <- data.frame(
        "_id" = ids,
        "json" = value,
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
    }
  }

  # data frame
  if (class(value) == "data.frame") {
    #
    if (is.na(match("_id", names(value)))) {
      # if no _id column
      if (!identical(rownames(value),
                     as.character(seq_len(nrow(value))))) {
        # use rownames as _id's and remove rownames
        value[["_id"]] <- row.names(value)
        row.names(value) <- NULL
      } else {
        # add canonical _id's
        value[["_id"]] <- uuid::UUIDgenerate(use.time = TRUE, n = nrow(value))
      }
    } # if no _id column
    #
    if (ncol(value) > 2L || (ncol(value) == 2L &&
                             !all(sort(names(value)) == c("_id", "json")))) {
      # convert if there is no json column yet
      value[["json"]] <- strsplit(
        jsonify::to_ndjson(
          value[, -match("_id", names(value)), drop = FALSE],
          unbox = TRUE),
        split = "\n"
      )[[1]]
      # remove original columns
      value <- value[, c("_id", "json"), drop = FALSE]
    }
    #
    if (class(value[["_id"]]) == "list") {
      # in case fromJSON created the _id column as list
      value[["_id"]] <- unlist(value[["_id"]])
    }
  } # if data.frame

  # insert data
  result <- try(
    DBI::dbWithTransaction(
      src$con, {
        DBI::dbAppendTable(
          conn = src$con,
          name = key,
          # canonical value: a data frame
          # with 2 columns, _id and json
          value = value)
      }),
    silent = TRUE)

  # prepare returns
  if (inherits(result, "try-error")) {
    error <- trimws(sub(".+: (.*?):.+", "\\1", result))
    warning(
      "Could not create some documents, reeason: ",
      unique(error), call. = FALSE, immediate. = TRUE)
    result <- 0L
  }
  return(sum(result))

}

## helpers --------------------------------------

existsMessage <- function(k) {
  message(paste0("Note: container '", k, "' already exists"))
}

isUrl <- function(x) {
  # check if x is may be an url
  return(grepl("^https?://", x))
}

isFile <- function(x) {
  # check if x is the name of a readable file
  out <- try(file.access(x, mode = 4) == 0L, silent = TRUE)
  return(!inherits(out, "try-error") && out)
}
