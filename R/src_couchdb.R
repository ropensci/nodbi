#' Setup a database connection
#'
#' @export
#' @param type One of localhost, cloudant, or iriscouch. This is what's used to determine
#' how to structure the URL to make the request.
#' @param port Port number
#' @param user Username, if any
#' @param pwd Password, if any
#' @examples \dontrun{
#' src_couchdb()
#' }
src_couchdb <- function(type = "localhost", port = 5984, user = NULL, pwd = NULL){
  if(type != "localhost") sofa::cushion(type, port = port, user = user, pwd = pwd)
  cush <- sofa::cushions()[[type]]
  info <- sofa::ping()
  dbs <- sofa::db_list()
#   defdb <- defaultdb(db)
#   dbinfo <- db_info(dbname = defdb)
#   if(!is.null(dbinfo$error)) db_create(dbname=defdb)
#   dbout <- sofa::db_info(dbname = defdb)
  structure(c(info, cush), class=c("src_couchdb","docdb_src"), type="couchdb", dbs=dbs)
}

#' @export
print.src_couchdb <- function(x, ...){
  cat(sprintf("src: %s %s [%s/%s]", attr(x, "type"), x$version, x$type, x$port), sep = "\n")
#   cat(sprintf("db: %s", attr(x, "dbname")), sep = "\n")
  cat(doc_wrap("databases: ", paste0(attr(x, "dbs"), collapse = ", "), width=80), "\n", sep = "")
}

doc_wrap <- function (..., indent = 0, width = getOption("width")){
  x <- paste0(..., collapse = "")
  wrapped <- strwrap(x, indent = indent, exdent = indent + 5, width = width)
  paste0(wrapped, collapse = "\n")
}

couchdb_default <- "docdbi"
defaultdb <- function(x = NULL) {
  if(is.null(x) || !is.character(x)) {
    couchdb_default
  } else {
    x
  }
}
