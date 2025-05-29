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
#' @details Uses \pkg{monoglite} as backend. \pkg{nodbi} creates or uses
#' a MongoDB collection, in which `nodbi` creates JSON documents.
#' If documents do not have root-level `_id`'s, UUID's are created as `_id`'s.
#' MongoDB but none of the other databases require to specify the container
#' already in the `src_mongo()` function.
#' For a benchmark, see <https://github.com/ropensci/nodbi#benchmark>
#'
#' @return A `nodbi` source object
#'
#' @examples \dontrun{
#' con <- src_mongo()
#' print(con)
#' }
src_mongo <- function(collection = "test", db = "test",
                      url = "mongodb://localhost", ...) {

  # check minimum version
  pkgNeeded("mongolite", "1.6")

  # create connection
  con <- mongolite::mongo(collection, db, url, ...)

  # potential security concern with
  # storing the full connection string
  structure(
    list(
      con = con,
      collection = collection,
      db = db,
      url = url,
      ...),
    class = c("src_mongo", "docdb_src"))
}

#' @export
print.src_mongo <- function(x, ...) {

  # rights may be insufficient to call info(),
  # hence try() block and fallback printout
  tmp <- try({
    srv <- x$con$info()
    srcInfo("MongoDB", srv$server$version, x$db, srv$stats$size)
  }, silent = TRUE)

  if (inherits(tmp, "try-error")) {
    srcInfo("MongoDB", NA, x$db, NA)
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
