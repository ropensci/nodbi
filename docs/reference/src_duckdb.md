# Setup a DuckDB database connection

Setup a DuckDB database connection

## Usage

``` r
src_duckdb(drv = duckdb::duckdb(), dbdir = attr(drv, "dbdir"), ...)
```

## Arguments

- drv:

  Object returned by `duckdb()`

- dbdir:

  Location for database files. Should be a path to an existing directory
  in the file system. With the default (or `""`), all data is kept in
  RAM.

- ...:

  Additional named parameters passed on to
  [`DBI::dbConnect()`](https://dbi.r-dbi.org/reference/dbConnect.html)

## Value

A `nodbi` source object

## Details

Uses [`duckdb::duckdb()`](https://r.duckdb.org/reference/duckdb.html) as
backend. nodbi creates or uses a DuckDB table, with columns `_id` and
`json` created and used by package `nodbi`, applying SQL functions as
per <https://duckdb.org/docs/extensions/json> to the `json` column. Each
row in the table represents a `JSON` document. Any root-level `_id` is
extracted from the document(s) and used for column `_id`, otherwise a
UUID is created as `_id`. The table is indexed on `_id`. For a
benchmark, see <https://github.com/ropensci/nodbi#benchmark>.

## Examples

``` r
if (FALSE) { # \dontrun{
con <- src_duckdb()
print(con)
} # }
```
