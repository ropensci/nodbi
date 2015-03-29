#' Create documents
#'
#' @export
#' @param src source object, result of call to src
#' @param key A key. See Details.
#' @param value A value
#' @param ... Ignored
#' @details Note that with etcd, you have to prefix a key with a forward slash.
#' @examples \dontrun{
#' conn <- src_couchdb()
#' library("jsonlite")
#' doc <- fromJSON("http://api.gbif.org/v1/species/2704179")
#' docdb_create(conn, doc)
#'
#' doc2 <- fromJSON("http://api.gbif.org/v1/species/2704174")
#' docdb_create(conn, doc2)
#'
#' key="mtcars2"
#' value=mtcars
#' docdb_create(src, key, value)
#' docdb_get(src, key)
#'
#' # etcd
#' src <- src_etcd()
#' docdb_create(src, "/hello", "world")
#' ## a data.frame
#' docdb_create(src, key = "/newmtcars7", value = mtcars)
#' docdb_get(src, "/newmtcars7")
#'
#' # Elasticsearch
#' src <- src_elasticsearch()
#' docdb_create(src, key = "mtcars", value = mtcars)
#' docdb_create(src, key = "iris", value = iris)
#' docdb_create(src, key = "diamonds_small", value = diamonds[1:3000L,])
#' }
docdb_create <- function(src, key, value, ...){
  UseMethod("docdb_create")
}

#' @export
docdb_create.src_couchdb <- function(src, key, value, ...){
  dbinfo <- sofa::db_info(dbname = key)
  if(!is.null(dbinfo$error)) sofa::db_create(dbname=key)
  sofa::bulk_create(doc = value, cushion = src$type, dbname = key)
}

#' @export
docdb_create.src_etcd <- function(src, key, value, ...){
  invisible(etseed::create(key = key, dir = TRUE))
  cl <- class(value)
  switch(cl,
         data.frame = {
           for(i in 1:NROW(value)){
             etseed::create(paste0(key, "/", i), jsonlite::toJSON(value[i,]))
           }
         }
  )
}

#' @export
docdb_create.src_elasticsearch <- function(src, key, value, ...){
  elastic::index_create(index = key, verbose = FALSE)
  cl <- class(value)
  switch(cl,
         data.frame = {
           ff <- paste0(key, ".json")
           make_bulk(key, value, ff)
           invisible(elastic::docs_bulk(ff))
         }
  )
}

# make_bulk("mtcars", mtcars, "~/mtcars.json")
# make_bulk("iris", iris, "~/iris.json")
# make_bulk("diamonds", diamonds, "~/diamonds.json")
make_bulk <- function(key, value, filename = "~/docdbi_bulk.json") {
  unlink(filename)
  for(i in 1:NROW(value)){
    dat <- list(index = list(`_index` = key, `_type` = key, `_id` = i-1))
    cat(proc_doc(dat), sep = "\n", file = filename, append = TRUE)
    cat(proc_doc(value[i,]), sep = "\n", file = filename, append = TRUE)
  }
}

proc_doc <- function(x){
  b <- jsonlite::toJSON(x, auto_unbox = TRUE)
  gsub("\\[|\\]", "", as.character(b))
}
