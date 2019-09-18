#' Setup database connections
#'
#' @name src
#' @details There is a `src_*()` function to setup a connection to each
#' of the database backends. Each has their own unique set of parameters.
#'
#' - MongoDB - [src_mongo()]
#' - CouchDB - [src_couchdb()]
#' - Elasticsearch - [src_elastic()]
#' - Redis - [src_redis()]
#' - SQLite - [src_sqlite()]
#'
#' Documentation details for each database:
#'
#' - MongoDB - <https://docs.mongodb.com/>
#' - CouchDB - <http://docs.couchdb.org/>
#' - Elasticsearch -
#'  <https://www.elastic.co/guide/en/elasticsearch/reference/current>
#' - Redis - <https://redis.io/documentation>
#' - SQLite/json1 - <https://www.sqlite.org/json1.html>
NULL
