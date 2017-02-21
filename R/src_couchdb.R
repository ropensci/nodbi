#' Setup a CouchDB database connection
#'
#' @export
#' @param type One of localhost, cloudant, or iriscouch. This is what's
#' used to determine how to structure the URL to make the request.
#' @param port Port number
#' @param user Username, if any
#' @param pwd Password, if any
#' @examples \dontrun{
#' src_couchdb()
#' }
src_couchdb <- function(host = "127.0.0.1", port = 5984, path = NULL,
                        transport = "http", user = NULL, pwd = NULL,
                        headers = NULL) {

  x <- sofa::Cushion$new(host = host, port = port)
  x$path <- path
  x$transport <- transport
  x$user <- user
  x$pwd <- pwd
  x$headers <- headers
  info <- sofa::ping(x)
  dbs <- sofa::db_list(x)

  structure(list(x),
            class = c("src_couchdb", "docdb_src"),
            type = "couchdb",
            info = info, dbs = dbs)
}

#' @export
print.src_couchdb <- function(x, ...) {
  info <- attr(x, "info")
  cat(sprintf("src: %s %s [%s/%s]", attr(x, "type"), info$version,
              x[[1]]$host, x[[1]]$port), sep = "\n")
  cat(doc_wrap("databases: ", paste0(attr(x, "dbs"), collapse = ", "),
               width = 80), "\n", sep = "")
}
