# nodbi helper functions



# provide private environment,
# e.g. for initTransformers()
#
.nodbi <- new.env()



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

}



#' .onUnload
#'
#' a session restart does not trigger this
#' @keywords internal
#' @noRd
#'
.onUnload <- function(libpath) {
  closeNodbiConnections(e = globalenv())
}

}
