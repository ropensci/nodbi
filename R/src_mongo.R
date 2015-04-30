#' Setup a mongoDB database connection
#' 
#' @param db Name of the database to use
#' @param url Server host url
#' @export
#' @examples
#' con <- src_mongo()
#' print(con)
src_mongo <- function(url = "mongodb://localhost", db = "test") {
  con <- mongolite:::mongo_client_new(url)
  structure(list(con = con, db = db), class = c("src_mongo", "docdb_src"))
}

##' @export
print.src_mongo <- function(x, ...) {
  con <- x$con
  db <- x$db
  srv <- mongolite:::mongo_client_server_status(con)
  cat(sprintf("MongoDB %s (uptime: %ss)\nURL: %s/%s", srv$version, srv$uptime, srv$host, db))
}
