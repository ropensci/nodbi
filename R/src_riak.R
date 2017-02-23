#' Setup a Riak database connection
#'
#' @export
#' @param host (character) host value, default: 127.0.0.1
#' @param port (integer/numeric) Port. Remember that if you don't want a port
#' set, set this parameter to NULL. Default: 8098
#' @param path (character) context path that is appended to the end of the url. e.g.,
#' bar in http://foo.com/bar. Default: NULL, ignored
#' @param transport (character) http or https. Default: http
#' @param user (character) Username, if any
#' @param pwd (character) Password, if any
#' @param headers (list) list of named headers
#' @examples \dontrun{
#' x <- src_riak()
#' }
src_riak <- function(host = "127.0.0.1", port = 8098, path = NULL,
                     transport = "http", user = NULL, pwd = NULL,
                     headers = NULL) {

  x <- reeack::riak(host = host, port = port, path = path,
                    transport = transport, user = user,
                    pwd = pwd, headers = headers)
  info <- x$stats()
  buckets <- x$buckets()$buckets

  structure(list(con = x),
            class = c("src_riak", "docdb_src"),
            type = "riak", host = host, port = port,
            info = info, dbs = buckets)
}

#' @export
print.src_riak <- function(x, ...) {
  info <- attr(x, "info")
  cat(sprintf("src: %s %s [%s/%s]", attr(x, "type"), info$riak_kv_version,
              attr(x, "host"), attr(x, "port")), sep = "\n")
  cat(doc_wrap("databases: ", paste0(attr(x, "dbs"), collapse = ", "),
               width = 80), "\n", sep = "")
}
