#' Setup a RMariaDB database connection
#'
#' @export
#'
#' @param dbname (character) name of database file,
#'   defaults to ":memory:" for an in-memory database,
#'   see [RMariaDB::MariaDB()]
#' @param ... additional named parameters passed
#'   on to [RMariaDB::MariaDB()]
#'
#' @details Uses \pkg{RSMariaDB} as backend; minimum MariaDB 12.3 required.
#' \pkg{nodbi} creates or uses
#' a table with columns `_id` and `json`, created and used by
#' package `nodbi`, applying SQL or JSON functions as per
#' https://mariadb.com/docs/server/reference/sql-functions/special-functions/json-functions
#' to the `json` column.
#' Each row in the table represents a `JSON` document.
#' Any root-level `_id` is extracted from the document(s) and used for
#' column `_id`, otherwise a UUID is created as `_id`.
#' The table is indexed on `_id`.
#' For a benchmark, see <https://github.com/ropensci/nodbi#benchmark>.
#'
#' @return A `nodbi` source object
#'
#' @examples \dontrun{
#' con <- src_sqlite()
#' print(con)
#' }
#'
src_mariadb <- function(dbname = "test", user = NULL, ...) {

  # check minimum version
  pkgNeeded("RMariaDB", "1.3.0")

  # open connection
  con <- DBI::dbConnect(
    drv = RMariaDB::MariaDB(),
    dbname = dbname,
    user = user,
    ...)

  # https://mariadb.com/docs/server/reference/sql-functions/special-functions/json-functions
  # https://mariadb.com/docs/server/reference/sql-functions/special-functions/json-functions/jsonpath-expressions

  # ensure disconnect
  reg.finalizer(
    e = globalenv(),
    f = closeNodbiConnections,
    onexit = TRUE
  )

  # get info
  info <- DBI::dbGetQuery(
    conn = con,
    statement = "SHOW VARIABLES;")

  # MySQLdb or MariaDB
  dbver <- info$Value[grepl("^innodb_version$|^version$", info$Variable_name)]
  dbver <- gsub("(.*?)-.*", "\\1", dbver)
  dbver <- gsub("[^.0-9]", "", dbver)
  dbver <- unique(dbver)[1]

  # check versions
  if (!package_version(dbver) > '12.3') stop(
    "Minimum version is 12.3, server reports ", dbver
  )

  # TODO
  # # stored function
  # statement <- r"(
  #      CREATE FUNCTION ExtractJsonWithParents(
  #           raw_json JSON,
  #           target_path VARCHAR(255)
  #         )
  #       RETURNS JSON
  #       DETERMINISTIC
  #
  #       BEGIN
  #       DECLARE target_value JSON;
  #       DECLARE working_path VARCHAR(255);
  #       DECLARE current_key VARCHAR(255);
  #       DECLARE output_json JSON;
  #       DECLARE dot_pos INT;
  #
  #         SET target_value = JSON_VALUE(raw_json, target_path);
  #         IF target_value IS NULL THEN
  #             SET target_value = JSON_EXTRACT(raw_json, target_path);
  #         END IF;
  #
  #         IF target_value IS NULL THEN
  #             RETURN NULL;
  #         END IF;
  #
  #         SET output_json = target_value;
  #         SET working_path = REPLACE(target_path, '$.', '');
  #
  #         WHILE LENGTH(working_path) > 0 DO
  #             SET dot_pos = LENGTH(working_path) - POSITION('.' IN REVERSE(working_path)) + 1;
  #
  #             IF POSITION('.' IN working_path) > 0 THEN
  #                 SET current_key = SUBSTRING(working_path, dot_pos + 1);
  #                 SET working_path = SUBSTRING(working_path, 1, dot_pos - 1);
  #             ELSE
  #                 SET current_key = working_path;
  #                 SET working_path = '';
  #             END IF;
  #
  #             SET current_key = TRIM(BOTH '"' FROM TRIM(BOTH '\'' FROM current_key));
  #
  #             IF current_key IS NOT NULL AND current_key != '' THEN
  #                 IF JSON_VALID(output_json) AND JSON_TYPE(output_json) IN ('OBJECT', 'ARRAY') THEN
  #                     SET output_json = JSON_OBJECT(current_key, JSON_QUERY(output_json, '$'));
  #                 ELSE
  #                     SET output_json = JSON_OBJECT(current_key, output_json);
  #                 END IF;
  #             END IF;
  #         END WHILE;
  #
  #         RETURN output_json;
  #     END ;)"
  #
  # # try setting stored function
  # out <- try(
  #   DBI::dbSendStatement(
  #     conn = con,
  #     statement = statement),
  #   silent = TRUE)
  #
  # if (inherits(out, "try-error") &&
  #     !grepl("already exists", out)) {
  #
  #   stop("Cannot store function in MariaDB '", dbname,
  #        "', but this is needed for nodbi::docdb_query()")
  #
  # }
  #
  # # administer stored function
  # try(DBI::dbSendStatement(
  #   conn = dbc$con,
  #   statement = paste0(
  #     "GRANT EXECUTE ON FUNCTION `", dbname,
  #     "`.ExtractJsonWithParents TO 'PUBLIC';")),
  #   silent = TRUE)
  #
  # # test stored function
  # out <- try(
  #   DBI::dbGetQuery(
  #     conn = con,
  #     statement = "
  #     SELECT ExtractJsonWithParents(
  #       '{\"a\": 1, \"b\": {\"c\": {\"d\": \"v\", \"e\": [0, 1]}}}',
  #       '$.b.c.d'
  #     ) AS result;
  #   "
  #   ),
  #   silent = TRUE
  # )
  # if (inherits(out, "try-error") || nrow(out) != 1L) stop(
  #   "Stored function not working or cannot be executed. ",
  #   "Have it removed and try again, please.")


  # return standard nodbi structure
  structure(
    list(
      con = con,
      dbname = dbname,
      dbver = dbver,
      user = user,
      ...),
    class = c("src_mariadb", "docdb_src"))

}

#' @export
print.src_mariadb <- function(x, ...) {

  dbsize <- NA_real_

  info <- try(
    suppressWarnings(
      DBI::dbGetQuery(
        conn = x$con,
        statement = paste0(
          'SELECT table_schema AS "DB", ',
          'SUM(data_length + index_length) \'B\' ',
          'FROM information_schema.tables;'
        )
      )
    )
  )

  if (!inherits(info, "try-error")) dbsize <- info$B

  srcInfo("MariaDB", x$dbver, x$dbname, dbsize)

}
