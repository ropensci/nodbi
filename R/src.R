#' Setup database connections
#'
#' @name src
#' @details There is a `src_*()` function to setup a connection to each
#' of the database backends. Each has their own unique set of parameters.
#'
#' - MongoDB - [src_mongo()]
#' - CouchDB - [src_couchdb()]
#' - etcd - [src_etcd()]
#' - Elasticsearch - [src_elastic()]
#' - Redis - [src_redis()]
#'
#' Documentation details for each database:
#'
#' - MongoDB - <https://docs.mongodb.com/>
#' - CouchDB - <http://docs.couchdb.org/>
#' - etcd - <https://coreos.com/etcd/docs/latest/getting-started-with-etcd.html>
#' - Elasticsearch - <https://www.elastic.co/guide/en/elasticsearch/reference/current>
#' - Redis - <https://redis.io/documentation>
NULL
