#' Setup a CouchDB database connection
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
  structure(c(info, cush), class=c("src_couchdb","docdb_src"), type="couchdb", dbs=dbs)
}

#' @export
print.src_couchdb <- function(x, ...){
  cat(sprintf("src: %s %s [%s/%s]", attr(x, "type"), x$version, x$type, x$port), sep = "\n")
  cat(doc_wrap("databases: ", paste0(attr(x, "dbs"), collapse = ", "), width=80), "\n", sep = "")
}
