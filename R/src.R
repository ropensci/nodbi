#' Setup database connections
#'
#' There is a `src_*()` function to setup a connection to each
#' of the database backends. The backends may have specific parameters
#' in the respective function `src_*()`, but all other `nodbi` functions
#' are independent of the backend (e.g., see [docdb_query()]).
#'
#' - MongoDB - [src_mongo()]
#' - SQLite - [src_sqlite()]
#' - Elasticsearch - [src_elastic()]
#' - CouchDB - [src_couchdb()]
#' - PostgreSQL - [src_postgres()]
#' - DuckDB - [src_duckdb()]
#'
#' Documentation details for each database:
#'
#' - MongoDB - \url{https://www.mongodb.com/docs/manual/crud/}
#' - SQLite/JSON1 - \url{https://www.sqlite.org/json1.html}
#' - Elasticsearch - \url{https://www.elastic.co/docs/get-started}
#' - CouchDB - \url{https://docs.couchdb.org/en/stable/api/index.html}
#' - PostgreSQL - \url{https://www.postgresql.org/docs/current/functions-json.html}
#' - DuckDB - \url{https://duckdb.org/docs/stable/data/json/overview.html}
#'
#' Documentation of R packages used by `nodbi` for the databases:
#'
#' - mongolite - \url{https://CRAN.R-project.org/package=mongolite}
#' - RSQLite - \url{https://CRAN.R-project.org/package=RSQLite}
#' - elastic - \url{https://CRAN.R-project.org/package=elastic}
#' - sofa - \url{https://CRAN.R-project.org/package=sofa}
#' - RPostgres - \url{https://CRAN.R-project.org/package=RPostgres}
#' - duckdb - \url{https://CRAN.R-project.org/package=duckdb}
#'
#' @name src
#' @docType data
#'
NULL
