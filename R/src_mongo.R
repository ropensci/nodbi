#' Setup a MongoDB database connection
#'
#' @export
#'
#' @param collection (character) Name of collection
#'
#' @param db (character) Name of database
#'
#' @param url (character) Address of the MongoDB server in Mongo connection
#' string URI format, see to [mongolite::mongo()]
#'
#' @param ... Additional named parameters passed on to [mongolite::mongo()]
#'
#' @details Uses \pkg{monoglite} under the hood; uses [mongolite::mongo()] for
#' connecting
#'
#' @examples \dontrun{
#' con <- src_mongo()
#' print(con)
#' }
src_mongo <- function(collection = "test", db = "test",
                      url = "mongodb://localhost", ...) {

  con <- mongolite::mongo(collection, db, url, ...)

  # potential security concern with
  # storing the full connection string
  structure(list(con = con,
                 collection = collection,
                 db = db,
                 url = url,
                 ...),
            class = c("src_mongo", "docdb_src"))
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
    cat(sprintf("MongoDB %s (uptime: %ss)\nURL: %s\nDatabase: %s\nCollection: %s\n",
                srv$server$version, srv$server$uptime, url, db, coll))
  }, silent = TRUE)

  if (inherits(tmp, "try-error")) {
    cat(sprintf("MongoDB \nURL: %s\nDatabase: %s\nCollection: %s\n",
                url, db, coll))

  }
}

#' @keywords internal
#' @noRd
chkSrcMongo <- function(src, key = NULL) {
  if (exists("key", inherits = FALSE) && src$collection != key) {
    warning("Parameter 'key' = '", key, "' differs from 'collection' = '",
            src$collection, "' that was specified in src_mongo();",
            " using the latter.", call. = FALSE, immediate. = FALSE)
  }
}
