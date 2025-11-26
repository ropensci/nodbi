# Setup a PostgreSQL database connection

Setup a PostgreSQL database connection

## Usage

``` r
src_postgres(dbname = "test", host = "localhost", port = 5432L, ...)
```

## Arguments

- dbname:

  (character) name of database, has to exist to open a connection

- host:

  (character) host of the database, see
  [`RPostgres::Postgres()`](https://rpostgres.r-dbi.org/reference/Postgres.html)

- port:

  (integer) port of the database, see
  [`RPostgres::Postgres()`](https://rpostgres.r-dbi.org/reference/Postgres.html)

- ...:

  additional named parameters passed on to
  [`RPostgres::Postgres()`](https://rpostgres.r-dbi.org/reference/Postgres.html)

## Value

A `nodbi` source object

## Details

Uses RPostgres as backend. nodbi creates or uses a PostgreSQL table,
with columns `_id` and `json` created and used by package `nodbi`,
applying SQL functions as per
<https://www.postgresql.org/docs/current/functions-json.html> to the
`json` column. Each row in the table represents a `JSON` document. Any
root-level `_id` is extracted from the document(s) and used for column
`_id`, otherwise a UUID is created as `_id`. The table is indexed on
`_id`. A custom `plpgsql` function is used for
[`docdb_update()`](https://docs.ropensci.org/nodbi/reference/docdb_update.md).
The order of variables in data frames returned by
[`docdb_get()`](https://docs.ropensci.org/nodbi/reference/docdb_get.md)
and
[`docdb_query()`](https://docs.ropensci.org/nodbi/reference/docdb_query.md)
can differ from their order the input to
[`docdb_create()`](https://docs.ropensci.org/nodbi/reference/docdb_create.md).
For a benchmark, see <https://github.com/ropensci/nodbi#benchmark>

## Examples

``` r
if (FALSE) { # \dontrun{
con <- src_postgres()
print(con)
} # }
```
