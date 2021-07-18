#' Setup a CouchDB database connection
#'
#' @export
#' @param host (character) host value, default: 127.0.0.1
#' @param port (integer/numeric) Port. Remember that if you don't want a port
#' set, set this parameter to NULL. Default: 5984
#' @param path (character) context path that is appended to the end
#' of the url. e.g., bar in http://foo.com/bar. Default: NULL, ignored
#' @param transport (character) http or https. Default: http
#' @param user (character) Username, if any
#' @param pwd (character) Password, if any
#' @param headers (list) list of named headers
#' @details uses \pkg{sofa} under the hood; uses [sofa::Cushion] for
#' connecting
#' @examples \dontrun{
#' src_couchdb()
#' }
src_couchdb <- function(host = "127.0.0.1", port = 5984, path = NULL,
                        transport = "http", user = NULL, pwd = NULL,
                        headers = NULL) {

  x <- sofa::Cushion$new(host = host,
                         port = port,
                         path = path,
                         transport = transport,
                         user = user,
                         pwd = pwd,
                         headers = headers)
  info <- sofa::ping(x)
  dbs <- sofa::db_list(x)

  structure(list(con = x),
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
