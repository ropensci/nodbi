#' Setup a mongoDB database connection
#'
#' @export
#' @param collection (character) name of collection
#' @param db (character) name of database
#' @param url (character) address of the mongodb server in mongo connection
#' string URI format.
#' @param ... additional named params passed on to [mongolite::mongo]
#' @details uses \pkg{monoglite} under the hood; uses [mongolite::mongo()] for
#' connecting
#' @examples \dontrun{
#' (con <- src_mongo())
#' print(con)
#' }
src_mongo <- function(collection = "test", db = "test",
                      url = "mongodb://localhost", ...) {
  
  con <- mongolite::mongo(collection, db, url, ...)
  structure(list(con = con, collection = collection, db = db, url = url), class = c("src_mongo", "docdb_src"))
}

#' @export
print.src_mongo <- function(x, ...) {
  con  <- x$con
  coll <- x$collection
  db   <- x$db
  url  <- x$url
  
  # rights may be insufficient to call info(),
  # hence try() block and fallback printout
  tmp <- try({
    srv <- con$info()
    cat(sprintf("MongoDB %s (uptime: %ss)\nURL: %s/%s \nCollection: %s\n",
                srv$server$version, srv$server$uptime, srv$server$host, db, coll))
  }, 
  silent = TRUE)
  
  if ("try-error" %in% class(tmp)) {
    cat(sprintf("MongoDB \nURL: %s/%s \nCollection: %s\n",
                url, db, coll))
    
  }
}
