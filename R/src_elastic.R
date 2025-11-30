#' Setup an Elasticsearch database connection
#'
#' @export
#'
#' @param host (character) the base url, defaults to localhost
#' (http://127.0.0.1)
#'
#' @param port (character) port to connect to, defaults to 9200 (optional)
#'
#' @param path (character) context path that is appended to the end of the
#' url. Default: `NULL`, ignored
#'
#' @param transport_schema (character) http or https. Default: http
#'
#' @param user (character) User name, if required for the connection. You
#' can specify, but ignored for now.
#'
#' @param pwd	(character) Password, if required for the connection. You can
#' specify, but ignored for now.
#'
#' @param force	(logical) Force re-load of connection details
#'
#' @param ...	Further args passed on to [elastic::connect()]
#'
#' @details Uses \pkg{elastic} as backend. \pkg{nodbi} creates or uses
#' an Elasticsearch index, in which `nodbi` creates JSON documents.
#' Any root-level `_id` is extracted from the document(s) and used as
#' document ID `_id`, otherwise a UUID is created as document ID `_id`.
#' Only lowercase is accepted for container names (in parameter `key`).
#' Opensearch can equally be used.
#' For a benchmark, see <https://github.com/ropensci/nodbi#benchmark>
#'
#' @return A `nodbi` source object
#'
#' @examples \dontrun{
#' con <- src_elastic()
#' print(con)
#' }
src_elastic <- function(host = "127.0.0.1", port = 9200, path = NULL,
                        transport_schema = "http", user = NULL, pwd = NULL,
                        force = FALSE, ...) {

  # check minimum version
  pkgNeeded("elastic", "1.2.0")

  # create connection
  x <- elastic::connect(
    host = host, port = port, path = path,
    transport_schema = transport_schema, user = user,
    pwd = pwd, force = force, ...)

  dbs <- names(elastic::index_stats(x)$indices)

  structure(
    list(
      con = x,
      info = x$ping(),
      dbs = dbs),
    type = "elasticsearch",
    class = c("src_elastic", "docdb_src")
  )

}

#' @export
print.src_elastic <- function(x, ...) {

  dbs <- names(elastic::index_stats(x$con)$indices)
  dbsize <- sapply(dbs, function(i) elastic::index_stats(x$con)$indices[[i]]$total$store$size_in_bytes)

  srcInfo("Elasticsearch", x$info$version$number, dbs, dbsize)

}
