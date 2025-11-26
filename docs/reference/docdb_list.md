# List containers in database

List containers in database

## Usage

``` r
docdb_list(src, ...)
```

## Arguments

- src:

  Source object, result of call to any of functions
  [`src_mongo()`](https://docs.ropensci.org/nodbi/reference/src_mongo.md),
  [`src_sqlite()`](https://docs.ropensci.org/nodbi/reference/src_sqlite.md),
  [`src_elastic()`](https://docs.ropensci.org/nodbi/reference/src_elastic.md),
  [`src_couchdb()`](https://docs.ropensci.org/nodbi/reference/src_couchdb.md)
  [`src_duckdb()`](https://docs.ropensci.org/nodbi/reference/src_duckdb.md)
  or
  [`src_postgres()`](https://docs.ropensci.org/nodbi/reference/src_postgres.md)

- ...:

  Passed to function
  [`DBI::dbListTables()`](https://dbi.r-dbi.org/reference/dbListTables.html)

## Value

Vector of names of containers that can be used as parameter `key` with
other functions such as
[`docdb_create()`](https://docs.ropensci.org/nodbi/reference/docdb_create.md).

## Examples

``` r
if (FALSE) { # \dontrun{
src <- src_sqlite()
docdb_create(src, "iris", iris)
docdb_list(src)
} # }
```
