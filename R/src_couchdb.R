#' Setup a CouchDB database connection
#'
#' @param host (character) host value, default: 127.0.0.1
#'
#' @param port (integer/numeric) Port. Remember that if you don't want a port
#' set, set this parameter to NULL. Default: 5984
#'
#' @param path (character) context path that is appended to the end
#' of the url, e.g., bar in http://foo.com/bar. Default: NULL, ignored
#'
#' @param transport (character) http or https. Default: http
#'
#' @param user (character) Username, if any
#'
#' @param pwd (character) Password, if any
#'
#' @param headers (list) list of named headers
#'
#' @details Uses \pkg{sofa} as backend. \pkg{nodbi} creates or uses
#' a CouchDB database with JSON documents. If documents do not have
#' root-level `_id`'s, UUID's are created as `_id`'s. Function
#' [docdb_update()] uses [jqr::jqr()] to implement patching JSON.
#' For a benchmark, see <https://github.com/ropensci/nodbi#benchmark>.
#'
#' @return A `nodbi` source object
#'
#' @export
#'
#' @examples \dontrun{
#' con <- src_couchdb()
#' print(con)
#' }
src_couchdb <- function(host = "127.0.0.1", port = 5984, path = NULL,
                        transport = "http", user = NULL, pwd = NULL,
                        headers = NULL) {

  # check minimum version
  pkgNeeded("sofa", "0.4.0")

  # create connection
  x <- sofa::Cushion$new(
    host = host,
    port = port,
    path = path,
    transport = transport,
    user = user,
    pwd = pwd,
    headers = headers)

  info <- sofa::ping(x)
  dbs <- sofa::db_list(x)

  structure(
    list(con = x),
    type = "couchdb",
    info = info,
    dbs = dbs,
    class = c("src_couchdb", "docdb_src")
  )
}

#' @export
print.src_couchdb <- function(x, ...) {

  info <- attr(x, "info")
  dbname <- sofa::db_list(x$con)
  dbsize <- sapply(dbname, function(i) sofa::db_info(x$con, dbname = i)$sizes$file)

  srcInfo("CouchDB", info$version, dbname, dbsize)

}
