#' Setup an Elasticsearch database connection
#'
#' @export
#' @param host (character) the base url, defaults to localhost
#' (http://127.0.0.1)
#' @param port (character) port to connect to, defaults to 9200 (optional)
#' @param path (character) context path that is appended to the end of the
#' url. Default: `NULL`, ignored
#' @param transport_schema (character) http or https. Default: http
#' @param user (character) User name, if required for the connection. You
#' can specify, but ignored for now.
#' @param pwd	(character) Password, if required for the connection. You can
#' specify, but ignored for now.
#' @param force	(logical) Force re-load of connection details
#' @param ...	Further args passed on to [elastic::connect()]
#' @details uses \pkg{elastic} under the hood; uses [elastic::connect()] for
#' connecting
#' @examples \dontrun{
#' src_elastic()
#' }
src_elastic <- function(host = "127.0.0.1", port = 9200, path = NULL,
                              transport_schema = "http", user = NULL,
                              pwd = NULL, force = FALSE, ...) {

  elastic::connect(es_host = host, es_port = port, es_path = path,
                   es_transport_schema = transport_schema, es_user = user,
                   es_pwd = pwd, force = force, ...)
  conninfo <- elastic::connection()
  info <- elastic::ping()
  dbs <- names(elastic::aliases_get())
  structure(list(es_info = info, es_conn = conninfo, dbs = dbs),
            class = c("src_elastic","docdb_src"),
            type = "elasticsearch")
}

#' @export
print.src_elastic <- function(x, ...) {
  cat(sprintf("src: elasticsearch %s [%s:%s]",
              x$es_info$version$number,
              x$es_conn$host, x$es_conn$port), sep = "\n")
  cat(doc_wrap("databases: ", paste0(x$dbs, collapse = ", "),
               width = 80), "\n", sep = "")
}
