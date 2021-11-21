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
