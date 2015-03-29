#' Setup a database connection for Elasticsearch
#'
#' @export
#' @param base the base url, defaults to localhost (http://127.0.0.1)
#' @param port	port to connect to, defaults to 9200 (optional)
#' @param user	User name, if required for the connection. You can specify, but ignored for now.
#' @param pwd	Password, if required for the connection. You can specify, but ignored for now.
#' @param key	An API key, ignored for now
#' @param force	Force re-load of connection details
#' @param ...	Further args passed on to print for the es_conn class.
#' @examples \dontrun{
#' src_elasticsearch()
#' }
src_elasticsearch <- function(base = "http://127.0.0.1", port = 9200, user = NULL,
                              pwd = NULL, key = NULL, force = FALSE, ...) {

  elastic::connect(es_base = base, es_port = port, es_user = user,
                   es_pwd = pwd, es_key = key, force = force, ...)
  conninfo <- elastic::connection()
  info <- elastic::ping()
  dbs <- names(elastic::aliases_get())
  structure(list(es_info=info, es_conn=conninfo, dbs=dbs), class=c("src_elasticsearch","docdb_src"), type="elasticsearch")
}

#' @export
print.src_elasticsearch <- function(x, ...){
  cat(sprintf("src: elasticsearch %s [%s:%s]", x$es_info$version$number, x$es_conn$base, x$es_conn$port), sep = "\n")
  cat(doc_wrap("databases: ", paste0(x$dbs, collapse = ", "), width=80), "\n", sep = "")
}
