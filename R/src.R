#' Setup database connections
#'
#' @name src
#' @details There is a \code{src_*()} function to setup a connection to each
#' of the database backends. Each has their own unique set of parameters.
#'
#' \itemize{
#'  \item Redis - \code{\link{src_rrlite}}
#'  \item CouchDB - \code{\link{src_couchdb}}
#'  \item Etcd - \code{\link{src_etcd}}
#'  \item Elasticsearch - \code{\link{src_elasticsearch}}
#' }
NULL
