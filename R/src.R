#' Setup database connections
#'
#' @name src
#' @details There is a `src_*()` function to setup a connection to each
#' of the database backends. Each has their own unique set of parameters.
#'
#' - MongoDB - [src_mongo()]
#' - SQLite - [src_sqlite()]
#' - Elasticsearch - [src_elastic()]
#' - CouchDB - [src_couchdb()]
#'
#' Documentation details for each database:
#'
#' - MongoDB - \url{https://docs.mongodb.com/}
#' - SQLite/JSON1 - \url{https://www.sqlite.org/json1.html}
#' - CouchDB - \url{http://docs.couchdb.org/}
#' - Elasticsearch -
#'  \url{https://www.elastic.co/guide/en/elasticsearch/reference/current/index.html}
#'
#'  Documentation for R packages used by `nodbi` for the databases:
#'
#'  - mongolite - \url{https://CRAN.R-project.org/package=mongolite}
#'  - RSQLite - \url{https://CRAN.R-project.org/package=RSQLite}
#'  - sofa - \url{https://CRAN.R-project.org/package=sofa}
#'  - elastic - \url{https://CRAN.R-project.org/package=elastic}
#'
NULL
