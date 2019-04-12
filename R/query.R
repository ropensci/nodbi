#' Get documents with a filtering query
#'
#' @export
#' @param src source object, result of call to src
#' @param key (chartacter) A key. ignored for mongo
#' @param query various. see Query section below.
#' @param ... Additional named parameters passed on to each package:
#' 
#' - CouchDB: passed on to [sofa::db_query()]
#' - Elasticsearch: passed on to [elastic::Search()]
#' - MongoDB: passed on to the `$find` method of \pkg{mongolite}
#' 
#' @template deets
#' @section What is expected for each source:
#' 
#' - CouchDB: a list, see docs for [sofa::db_query()]
#' - Elasticsearch: query parameters, see [elastic::Search()]; passed to 
#' the `query` parameter of `elastic::Search`, thus performs a URI 
#' based search where the query is passed in the URI instead of the body.
#' In theory you can instead pass in a JSON or list to the `body`
#' parameter, but if you want to do complicated Elasticsearch queries
#' you may be better of using \pkg{elastic} package directly
#' - MongoDB: query parameters, see \pkg{mongolite} docs for 
#' help with searches
#' 
#' @section Not supported yet:
#' 
#' - Etcd
#' - Redis
#' 
#' @examples \dontrun{
#' # CouchDB
#' (src <- src_couchdb())
#' if (docdb_exists(src, "mtcars2")) docdb_delete(src, "mtcars2")
#' invisible(docdb_create(src, key = "mtcars2", value = mtcars))
#' docdb_exists(src, "mtcars2")
#' (query <- list(cyl = list("$gt" = 6)))
#' docdb_query(src, "mtcars2", query = query)
#'
#' # Elasticsearch
#' src <- src_elastic()
#' if (docdb_exists(src, "iris")) docdb_delete(src, "iris")
#' docdb_create(src, "iris", iris)
#' docdb_exists(src, "iris")
#' docdb_query(src, "iris", query = "setosa")
#' docdb_query(src, "iris", query = "1.5")
#' docdb_query(src, "iris", query = "Petal.Width:1.5")
#'
#' # Mongo
#' src <- src_mongo()
#' if (docdb_exists(src, "mtcars")) docdb_delete(src, "mtcars")
#' docdb_create(src, "mtcars", mtcars)
#' docdb_query(src, query = '{"mpg":21}')
#' docdb_query(src, query = '{"mpg":21}', fields = '{"mpg":1, "cyl":1}')
#' }
docdb_query <- function(src, key, query, ...){
  UseMethod("docdb_query")
}

#' @export
docdb_query.default <- function(src, key, query, ...) {
  stop("docdb_query supported for CouchDB, Elasticsearch & MongoDB")
}

#' @export
docdb_query.src_couchdb <- function(src, key, query, ...) {
  assert(key, 'character')
  dropmeta(makedf(
    sofa::db_query(src$con, dbname = key,
      selector = query, limit = 10, ...)$docs))
}

#' @export
docdb_query.src_elastic <- function(src, key, query, ...) {
  assert(key, 'character')
  ids <- pluck(elastic::Search(src$con, key, q = query, source = FALSE,
                               size = 10, ...)$hits$hits, "_id", "")
  if (length(ids) == 0) return(data.frame(NULL))
  tmp <- elastic::docs_mget(src$con, index = key, type = key, ids = ids,
                            verbose = FALSE)
  makedf(pluck(tmp$docs, "_source"))
}

#' @export
docdb_query.src_mongo <- function(src, key, query, ...) {
  src$con$find(query = query, ...)
}
