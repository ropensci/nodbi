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
#' @details uses \pkg{RPostgres} under the hood
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
  con <- DBI::dbConnect(
    drv = RPostgres::Postgres(),
    dbname = dbname,
    host = host,
    port = port,
    ...)

  # Manually disconnecting a connection is not necessary with RPostgres,
  # but still recommended; if you delete the object containing the connection,
  # it will be automatically disconnected during the next GC with a warning.

  # add plpgsql function for docdb_update(), which should only raise
  # an error if the plpgsql extension is not installed in dbname
  if (inherits(try(
    # credits: Joao Haas, https://stackoverflow.com/a/65093455
    DBI::dbExecute(conn = con, statement = 'CREATE OR REPLACE FUNCTION
     jsonb_merge_patch("target" jsonb, "patch" jsonb)
     RETURNS jsonb AS $$
    BEGIN
     RETURN COALESCE(jsonb_object_agg(
      COALESCE("tkey", "pkey"),
      CASE
       WHEN "tval" ISNULL THEN "pval"
       WHEN "pval" ISNULL THEN "tval"
       WHEN jsonb_typeof("tval") != \'object\'
        OR jsonb_typeof("pval") != \'object\' THEN "pval"
       ELSE jsonb_merge_patch("tval", "pval")
     END
     ), \'{}\'::jsonb)
    FROM jsonb_each("target") e1("tkey", "tval")
    FULL JOIN jsonb_each("patch") e2("pkey", "pval")
     ON "tkey" = "pkey"
     WHERE jsonb_typeof("pval") != \'null\' OR "pval" ISNULL;
    END;
   $$ LANGUAGE plpgsql;
  '), silent = TRUE), "try-error")) {
    stop("PostgreSQL does not support plpgsql in table '", dbname,
         "', but this is needed for nodbi::docdb_update()")
  }

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
