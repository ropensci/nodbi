#' Setup a PostgreSQL database connection
#'
#' @export
#'
#' @param dbname (character) name of database,
#'   has to exist to open a connection
#'
#' @param host (character) host of the database,
#'  see [RPostgres::Postgres()]
#'
#' @param port (integer) port of the database,
#'  see [RPostgres::Postgres()]
#'
#' @param ... additional named parameters passed
#'  on to [RPostgres::Postgres()]
#'
#' @details Uses \pkg{RPostgres} as backend. \pkg{nodbi} creates or uses
#' a PostgreSQL table, with columns `_id` and `json` created and used
#' by package `nodbi`, applying SQL functions as per
#' <https://www.postgresql.org/docs/current/functions-json.html>
#' to the `json` column.
#' Each row in the table represents a `JSON` document.
#' Any root-level `_id` is extracted from the document(s) and used
#' for column `_id`, otherwise a UUID is created as `_id`.
#' The table is indexed on `_id`. A custom `plpgsql` function
#' is used for `docdb_update()`.
#' The order of variables in data frames returned by `docdb_get()`
#' and `docdb_query()` can differ from their order the input to
#' `docdb_create()`.
#' For a benchmark, see <https://github.com/ropensci/nodbi#benchmark>
#'
#' @return A `nodbi` source object
#'
#' @examples \dontrun{
#' con <- src_postgres()
#' print(con)
#' }
#'
src_postgres <- function(dbname = "test",
                         host = "localhost",
                         port = 5432L,
                         ...) {

  # open connection
  con <- try(DBI::dbConnect(
    drv = RPostgres::Postgres(),
    dbname = dbname,
    host = host,
    port = port,
    ...),
    silent = TRUE)

  # inform user on missing database
  if (inherits(con, "try-error")) {
    if (grepl("database .+ does not exist", con)) {
      con <- paste0(
        "Database '", dbname, "' has to be created first, see here ",
        "https://www.postgresql.org/docs/current/manage-ag-createdb.html. ",
        "Possibly, from within this R session, this might work: ",
        'system2("createdb", "', dbname, '") Note that Homebrew installs ',
        'may need system2("createdb-<major PostgreSQL version number>", "',
        dbname, '")'
      )
    }
    stop(con, call. = FALSE)
  }

  # Manually disconnecting a connection is not necessary with RPostgres,
  # but still recommended; if you delete the object containing the connection,
  # it will be automatically disconnected during the next GC with a warning.

  # add plpgsql function for docdb_update(), which should only raise
  # an error if the plpgsql extension is not installed in dbname
  out <- try(
    DBI::dbExecute(conn = con, statement = "
      CREATE OR REPLACE FUNCTION jsonb_patch(base jsonb, patch jsonb)
      RETURNS jsonb LANGUAGE plpgsql AS $$
      BEGIN
        RETURN (
          SELECT COALESCE(
            JSONB_OBJECT_AGG(
              COALESCE(k1, k2),
              CASE
                WHEN v1 IS NULL THEN v2
                WHEN v2 IS NULL THEN v1
                WHEN jsonb_typeof(v1) = 'object' AND
                     jsonb_typeof(v2) = 'object'
                THEN jsonb_patch(v1, v2)
                ELSE v2
              END
            ), '{}'::jsonb)
          FROM jsonb_each(base) AS b(k1, v1)
          FULL JOIN jsonb_each(patch) AS p(k2, v2) ON k1 = k2
          WHERE jsonb_typeof(v2) <> 'null' OR v2 IS NULL
        );
      END;
      $$;"), silent = TRUE)

  if (inherits(out, "try-error") &&
      !grepl("tuple concurrently updated", out)) {

    stop("PostgreSQL does not support plpgsql in table '", dbname,
         "', but this is needed for nodbi::docdb_update()")
  }

  # ensure disconnect
  reg.finalizer(
    e = globalenv(),
    f = closeNodbiConnections,
    onexit = TRUE
  )

  # return standard nodbi structure
  structure(list(con = con,
                 dbname = dbname,
                 host = host,
                 port = port,
                 ...),
            class = c("src_postgres", "docdb_src"))

}

#' @export
print.src_postgres <- function(x, ...) {

  # provide short info
  cat(sprintf(
    "src: PostgreSQL\ndbname: %s\n",
    x$dbname))

}
