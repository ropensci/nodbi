pluck <- function(x, name, type) {
  if (missing(type)) {
    lapply(x, "[[", name)
  } else {
    vapply(x, "[[", name, FUN.VALUE = type)
  }
}

doc_wrap <- function(..., indent = 0, width = getOption("width")) {
  x <- paste0(..., collapse = "")
  wrapped <- strwrap(x, indent = indent, exdent = indent + 5, width = width)
  paste0(wrapped, collapse = "\n")
}

makedf <- function(x) {
  (xyz <-
     data.table::setDF(data.table::rbindlist(x, use.names = TRUE, fill = TRUE)))
}

assert <- function(x, y) {
  if (!is.null(x)) {
    # if (!class(x) %in% y) {
    if (!inherits(x, y)) {
      stop(deparse(substitute(x)), " must be of class ",
           paste0(y, collapse = ", "), call. = FALSE)
    }
  }
}

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
