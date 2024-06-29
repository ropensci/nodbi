# nodbi helper functions

#### variables ####

# provide private environment,
# e.g. for initTransformers()
#
.nodbi <- new.env()

# jq script for extracting field names
#
# The unique function takes as input an array and
# produces an array of the same elements, in sorted
# order, with duplicates removed
#
# However, field names are generated for each
# document in the input, they are not deduplicated
# across documents. The latter would require jqr
# to be able to handle jq's 'inputs' and flag "-n".
jqFieldNames <- '[ path(..) | map(select(type == "string")) | join(".") ] | unique[] '



#' doc_wrap
#'
#' used with couchdb, elastic
#'
#' @keywords internal
#' @noRd
#'
doc_wrap <- function(..., indent = 0, width = getOption("width")) {
  x <- paste0(..., collapse = "")
  wrapped <- strwrap(x, indent = indent, exdent = indent + 5L, width = width)
  paste0(wrapped, collapse = "\n")
}



#' assert
#'
#' used across nodbi
#'
#' @keywords internal
#' @noRd
#'
assert <- function(x, y) {
  if (!is.null(x)) {
    if (!any(class(x) %in% y)) {
      stop(deparse(substitute(x)), " must be one of class: ",
           paste0(y, collapse = ", "), call. = FALSE)
    }
  }
}



#' closeNodbiConnections
#'
#' ensure closing database connection(s)
#'
#' @keywords internal
#' @noRd
#'
closeNodbiConnections <- function(e) {

  # this function is called by .onLoad, .onUnload, and
  # from reg.finalizer in src_{sqlite,postgres,duckdb}

  # search environment for docdb_src connections
  objIsNodbiConnection <- sapply(
    ls(e), function(i)
      any(class(eval(parse(text = i))) == "docdb_src"),
    USE.NAMES = TRUE
  )
  if (!length(objIsNodbiConnection)) return(invisible(NULL))
  objIsNodbiConnection <- objIsNodbiConnection[objIsNodbiConnection]

  # disconnect helper function
  nodbiDisconnect <- function(objName) {

    # get duckdb driver information
    ddbdrv <- attr(eval(parse(text = objName))$con, "driver")

    # close valid, and also invalid connections such as
    # needed for DuckDB where then driver is not null
    if (DBI::dbIsValid(eval(parse(text = objName))$con) ||
        (!is.null(ddbdrv) && DBI::dbIsValid(ddbdrv))) {

      # disconnect and shutdown if needed
      res <- try(suppressWarnings(
        DBI::dbDisconnect(
          eval(parse(text = objName))$con,
          # duckdb needs to be shut down; parameter
          # does not adversely affect other backends
          shutdown = TRUE)),
        silent = TRUE)

      # inform user
      if (!inherits(res, "try-error") && res)
        message("nodbi: docdb_src '", objName, "' disconnected and shut down. ")

    }
  }

  # iterate over connections
  for (i in seq_along(objIsNodbiConnection)) {

    # get name of connection object
    objName <- names(objIsNodbiConnection[i])

    # run disconnect
    switch(
      # class is e.g., src_duckdb docdb_src
      class(eval(parse(text = objName)))[1],
      "src_duckdb" = nodbiDisconnect(objName),
      "src_sqlite" = nodbiDisconnect(objName),
      "src_postgres" = nodbiDisconnect(objName),
      NULL
    )
  }

}



#' .onLoad
#'
#' set up handler before database is accessed
#' this is triggered e.g. by session restart
#'
#' @keywords internal
#' @noRd
#'
.onLoad <- function(libname, pkgname) {

  # register closing our connections
  reg.finalizer(
    e = globalenv(),
    f = closeNodbiConnections,
    onexit = TRUE
  )

  # load javascript
  initTransformers()

}



#' .onUnload
#'
#' a session restart does not trigger this
#'
#' @keywords internal
#' @noRd
#'
.onUnload <- function(libpath) {

  closeNodbiConnections(e = globalenv())

}



#' initTransformers
#'
#' provide access to javascript functions and modules
#' stored in inst/js or subdir js of installed package
#'
#' https://cran.r-project.org/web/packages/V8/vignettes/npm.html
#'
#' purpose of javascript: transform mongo-like query into SQL
#'
#' @importFrom V8 v8 JS
#' @keywords internal
#' @noRd
#'
initTransformers <- function() {

  # early exit
  if (length(.nodbi)) return(NULL)

  # prepare V8, see ./inst/js/
  ct <- V8::v8()

  # get javascript
  ct$source(system.file("js/bundle.js", package = "nodbi"))

  # expects mdb to be db.user.find('{}')
  ct$assign("mongo2sql", V8::JS("function(mdb) {out = injs.convertToSQL(mdb); return out;}"))

  # assign into package private environment, see zzz.R
  assign("ct", ct, envir = .nodbi)

  # debug
  if (options()[["verbose"]]) {
    message("\nJS initiated\n")
  }

  # exit
  invisible(NULL)

}



#' digestFields
#'
#' takes input from "fields" and returns:
#' - longest common path for a minimum number of path elements
#'   for use with SELECT and possibly WHERE
#' - fields to be included
#' - fields to be excluded
#'
#' @importFrom stringi stri_match_all_regex
#' @keywords internal
#' @noRd
#'
digestFields <- function(f, q) {

  # check parameter
  if (is.null(f)) f <- "{}"
  f <- jsonlite::minify(f)

  # translate q into SQL query syntax using mongo2sql
  initTransformers()

  # - used:
  # $gt, $gte, $lt, $lte, $ne
  # $nin, $in, $regex,
  # $not, $and, $or, $nor

  # - not used:
  # $geoIntersects, $geoWithin,
  # $mod, $exists, $size, $nearSphere, $near
  # $text, $all, $where, $comment,
  # $meta, $slice, $elemMatch

  # stop if unused operators are in query
  usedOps <- c("$options", "$eq", "$gt", "$gte", "$lt", "$lte",
               "$ne", "$in", "$regex", "$and", "$or", "$nor")
  qOps <- stringi::stri_extract_all_regex(q, "(\\$[a-z]+)")[[1]]
  if (!all(is.na(qOps)) && !all(qOps %in% usedOps))  {
    stop("nodbi only supports: ", paste0(usedOps[-1], collapse = " / "),
         "; this was the query used: ", q)
  }

  sqlQ <- .nodbi$ct$call("mongo2sql", paste0("db.user.find(", q, ");"))

  # query mangling

  queryFields <- unique(stats::na.omit(stringi::stri_match_all_regex(
    sqlQ, '"([-@._\\w]+?)"')[[1]][, 2, drop = TRUE]))

  if (!length(queryFields) & q != "{}") stop(
    "Parameter 'query' did not reference any fields:\n", q)

  queryRootFields <- gsub("[.].*", "", queryFields)

  queryPaths <- character(0L)
  queryCondition <- character(0L)

  if (length(queryFields)) {

    # SELECT * FROM user WHERE <extract this>;
    queryCondition <- sub(".+? WHERE (.+);", "\\1", sqlQ)

    # "a.b" to "a"."b"
    queryCondition <- stringi::stri_replace_all_fixed(
      queryCondition, queryFields, gsub("[.]", '"."', queryFields),
      vectorize_all = FALSE
    )
    queryPaths <- gsub("[.]", '"."', queryFields)

    # = to ==
    queryCondition <- sub(" = ", " == ", queryCondition)

  }

  # fields mangling

  includeFields <- unique(stats::na.omit(stringi::stri_match_all_regex(
    f, '"([-@._\\w]+?)":[ ]*1')[[1]][, 2, drop = TRUE]))

  includeRootFields <- unique(gsub("[.].*", "", includeFields))
  includeRootFields <- includeRootFields[includeRootFields != "_id"]

  includeMaxCharFields <- sapply(
    includeFields, function(i) {
      if (!grepl(".", i, fixed = TRUE)) return(i)
      if (nchar(i) <= 63L) return(i)
      locDot <- substring(i, 1L, 63L) # 63L bytes is maximum for postgres
      # https://www.postgresql.org/docs/current/sql-syntax-lexical.html#SQL-SYNTAX-IDENTIFIERS
      locDot <- stringi::stri_locate_last_fixed(locDot, ".")[1, "start", drop = TRUE]
      substring(i, 1L, locDot - 1L)
    }, USE.NAMES = FALSE)

  excludeFields <- unique(stats::na.omit(stringi::stri_match_all_regex(
    f, '"([-@._\\w]+?)":[ ]*0')[[1]][, 2, drop = TRUE]))

  fieldStrings <- unique(c(includeFields, excludeFields))

  # translate mongo query into jq script to filter and select:

  # {"$or": [{"rows.elements.status": "OK"},
  # {"$and": [{"age": {"$gt": 21}},
  #           {"friends.name": {"$regex": "^B[a-z]{3,9}.*"}}
  #           {"_id": {"$in": ["5cd678531b423d5f04cfb0a1", "5cd678530df22d3625ed8375"]}]}]}
  #
  # ->
  #
  # def flatted: [paths(scalars) as $path | { ($path | map(tostring) | join("#")):
  # getpath($path) } ] | add; . | flatted as $f | . | select(
  #  ($f | with_entries( select( .key | match( "^rows#[0-9]+#elements#[0-9]+#status" ))) |
  #        map(select( . == "OK" )) | any )
  #  or
  #  (
  #   ($f | with_entries( select( .key | match( "^age" ))) | map(select(. > 20)) | any )
  #  and
  #   ($f | with_entries( select( .key | match( "^friends#[0-9]+#name" ))) |
  #         map(select(. | test("^B[a-z]{3,9}.*"))) | any )
  #  and
  #   ($f | with_entries( select( .key | match( "^_id" ))) |
  #    map(select( . | tostring | test("5|4") )) | any )
  #  )
  # )

  queryJq <- gsub("'", '"', queryCondition)
  # ugly but robust
  for (i in queryPaths) {

    # - handle IN since this uses brackets around argument
    xtr <- stringi::stri_extract_all_regex(
      queryJq, paste0("(\"", i, "\") IN (\\(.+?\\))"))[[1]]

    if (!all(is.na(xtr))) {

      xtr <- stringi::stri_replace_all_regex(
        xtr, paste0("(\"", i, "\") IN \\((.+?)\\)"), "$2")[[1]]

      # split on comma after number or double quote, avoid splitting on comma in string
      xtr <- strsplit(gsub("([0-9\"]),", "\\1@", xtr), "@")[[1]]

      # recompose
      xtr <- paste0(" . == ", xtr, collapse = " or ")

      # insert
      queryJq <- stringi::stri_replace_all_regex(
        queryJq,
        paste0("(\"", i, "\") IN (\\(.+?\\))", # brackets for IN
               "( AND | NOT | OR |\\)*$)"),
        paste0(" (\\$f | with_entries(select( .key | match( \"^",
               gsub('"[.]"', "#[0-9]+#", i),
               "\" ))) | map(select(", xtr, ")) | any ) $3"),
        vectorize_all = FALSE
      )
    }

    # - default operator handling
    queryJq <- stringi::stri_replace_all_regex(
      queryJq,
      paste0("(\"", i, "\") ([INOTREGXP=!<>']+ .+?)",
             # no extra bracket here
             "( AND | NOT | OR |\\)*$)"),
      paste0(" (\\$f | with_entries(select( .key | match( \"^",
             gsub('"[.]"', "#[0-9]+#", i),
             "\" ))) | map(select(. $2 )) | any ) $3"),
      vectorize_all = FALSE
    )

  }

  queryJq <- gsub(" ==* ", " == ", queryJq) # important
  # https://jqlang.github.io/jq/manual/#test
  # https://jqlang.github.io/jq/manual/#regular-expressions
  queryJq <- gsub("REGEXP \"(.+?)\"", '| test("\\1")', queryJq)
  queryJq <- gsub("( AND | NOT | OR )", "\\L\\1", queryJq, perl = TRUE)

  queryJq <- paste0('
     def flatted: [paths(scalars) as $path | { ($path | map(tostring) | join("#")):
     getpath($path) } ] | add; . | flatted as $f | select(', queryJq, ')')


  # output
  return(list(
    # vector of fields
    includeFields = includeFields,
    includeRootFields = includeRootFields,
    includeMaxCharFields = includeMaxCharFields,
    excludeFields = excludeFields,
    queryRootFields = queryRootFields,
    queryFields = queryFields,
    queryPaths = queryPaths,
    queryCondition = queryCondition,
    queryJq = queryJq
  ))

}



#' insObj
#'
#' replaces names of objects within sql quotes
#' `/** **/` by contents of objects of that name
#' found in the calling environment. Also adds
#' brackets where found.
#'
#' @keywords internal
#' @noRd
#'
insObj <- function(x, p = parent.frame(), e = NULL) {

  x <- gsub("\n+", " ", x)
  x <- gsub("  +", " ", x)

  allFound <- stringi::stri_extract_all_regex(x, "(/[*][*].*?[*][*]/)", simplify = FALSE)[[1]]

  if (setequal(allFound, e)) return(x)
  if (all(is.na(allFound))) return(x)

  for (oneFound in unique(allFound)) {

    i <- stringi::stri_replace_all_fixed(oneFound, c("/**", "**/"), "", vectorize_all = FALSE)
    i <- trimws(i)
    b <- stringi::stri_extract_all_fixed(i, c("'", '"'), simplify = FALSE)
    b <- unique(stats::na.omit(unlist(b)))

    i <- gsub("'|\"", "", i)
    if (grepl("[$]", i)) { # handle list
      ii <- sub(".+[$](.+)", "\\1", i)
      c <- get(sub("(.+)[$].+", "\\1", i), envir = p)[[ii]]
    } else {
      c <- get(i, envir = p)
    }

    if (!is.null(c) && length(c)) {
      if (length(c) > 1L) stop(
        call. = FALSE,
        "Objects should be atomic character vectors, this is not: ", i)
      if (length(b)) c <- paste0(b, c, b, collapse = "")
      x <- stringi::stri_replace_all_fixed(x, oneFound, c)
    }
  }

  # recurse
  insObj(x = x, p = p, e = allFound)
}


# for use in query.R
utils::globalVariables("jqFieldNames")
