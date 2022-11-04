# helper functions

# used with redis, couchdb, elastic
doc_wrap <- function(..., indent = 0, width = getOption("width")) {
  x <- paste0(..., collapse = "")
  wrapped <- strwrap(x, indent = indent, exdent = indent + 5L, width = width)
  paste0(wrapped, collapse = "\n")
}

# used across
assert <- function(x, y) {
  if (!is.null(x)) {
    if (!any(class(x) %in% y)) {
      stop(deparse(substitute(x)), " must be one of class: ",
           paste0(y, collapse = ", "), call. = FALSE)
    }
  }
}

# used with sqlite
dbWithTransaction <- function(conn, statement) {

  rollback <- function(e) {
    call <- DBI::dbExecute(conn, "ROLLBACK")
    if (identical(call, FALSE)) {
      stop(
        paste0(
          "Failed to rollback transaction. ",
          "Tried to roll back because an error occurred: ",
          conditionMessage(e)
        ),
        call. = FALSE
      )
    }
    if (inherits(e, "error")) {
      stop(e)
    }
  }

  call <- DBI::dbExecute(conn, "BEGIN IMMEDIATE")
  if (identical(call, FALSE)) {
    stop("Failed to begin transaction", call. = FALSE)
  }

  tryCatch({
    res <- force(statement)
    call <- DBI::dbExecute(conn, "COMMIT")
    if (identical(call, FALSE)) {
      stop("Failed to commit transaction", call. = FALSE)
    }
    res
  },
  db_abort = rollback,
  error = rollback,
  interrupt = rollback
  )

}

# manage clsoing database connection(s)
closeNodbiConnections <- function(e) {

  # message("nodbi running closeNodbiConnections()")

  # search environment for docdb_src connections
  objIsNodbiConnection <- sapply(
    ls(e), function(i)
      any(class(eval(parse(text = i))) == "docdb_src"),
    USE.NAMES = TRUE
  )
  if (!length(objIsNodbiConnection)) return(invisible(NULL))
  objIsNodbiConnection <- objIsNodbiConnection[objIsNodbiConnection]

  # disconnect
  nodbiDisconnect <- function(objName) {

    # get duckdb driver information
    ddbdrv <- attr(eval(parse(text = objName))$con, "driver")

    #
    if (DBI::dbIsValid(eval(parse(text = objName))$con) ||
        # duckdb needs to be shutdown
        (!is.null(ddbdrv) && DBI::dbIsValid(ddbdrv))) {

      # disconnect and shutdown if needed
      res <- try(suppressWarnings(
        DBI::dbDisconnect(
          eval(parse(text = objName))$con,
          # duckdb needs to be shut down
          shutdown = TRUE)),
        silent = TRUE)

      # inform user
      if (!inherits(res, "try-error") && res)
        message("nodbi: docdb_src '", objName, "' disconnected, shut down. ")

    }
  }

  # iterate over connections
  for (i in seq_along(objIsNodbiConnection)) {

    # get name of object
    objName <- names(objIsNodbiConnection[i])

    # run disconnect
    switch(
      class(eval(parse(text = objName)))[1],
      "src_duckdb" = nodbiDisconnect(objName),
      "src_sqlite" = nodbiDisconnect(objName),
      "src_postgres" = nodbiDisconnect(objName),
      NULL
    )

  }
}

# set up generic handler before database is accessed
# this is triggered e.g. by session restart
.onLoad <- function(libname, pkgname) {

  reg.finalizer(
    e = globalenv(),
    f = closeNodbiConnections,
    onexit = TRUE
  )

  # message("nodbi .onLoad reg.finalizer registered")

}

# a session restart does not trigger this
.onUnload <- function(libpath) {

  # message("nodbi running .onUnload()")

  closeNodbiConnections(e = globalenv())

}
