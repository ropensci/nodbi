#' Setup a database connection
#'
#' @importFrom sofa cushions cushion ping db_list
#' @name src
#' @param host Host url
#' @param port Port number
#' @param user Username, if any
#' @param pwd Password, if any
#' @param key API key, if any
#' @param x Input to print method
#' @param type One of localhost, cloudant, or iriscouch. This is what's used to determine
#' how to structure the URL to make the request.
#' @param ... further args passed on, or ignored
#' @examples \dontrun{
#' src_couchdb()
#' }

#' @export
#' @rdname src
src_couchdb <- function(type = "localhost", port = 5984, user = NULL, pwd = NULL){
  if(type != "localhost") cushion(type, port = port, user = user, pwd = pwd)
  cush <- cushions()[[type]]
  info <- sofa::ping()
  dbs <- sofa::db_list()
  structure(c(info, cush), class=c("src_couchdb","src"), type="couchdb", dbs=dbs)
}

#' @export
print.src_couchdb <- function(x, ...){
  cat(sprintf("src: %s %s [%s/%s]", attr(x, "type"), x$version, x$type, x$port), sep = "\n")
  cat(doc_wrap("databases: ", paste0(attr(x, "dbs"), collapse = ", "), width=80), "\n", sep = "")
}

doc_wrap <- function (..., indent = 0, width = getOption("width")){
  x <- paste0(..., collapse = "")
  wrapped <- strwrap(x, indent = indent, exdent = indent + 5, width = width)
  paste0(wrapped, collapse = "\n")
}
