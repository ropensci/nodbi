# Check if container exists in database

Check if container exists in database

## Usage

``` r
docdb_exists(src, key, ...)
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

- key:

  (character) The name of the container in the database backend
  (corresponds to `collection` for MongoDB, `dbname` for CouchDB,
  `index` for Elasticsearch, and to a table name for DuckDB, SQLite and
  PostgreSQL)

- ...:

  Passed to functions
  [`DBI::dbListTables()`](https://dbi.r-dbi.org/reference/dbListTables.html),
  [`elastic::index_exists()`](https://docs.ropensci.org/elastic/reference/indices.html),
  and
  [`sofa::db_info()`](https://docs.ropensci.org/sofa/reference/db_info.html)

## Value

(logical) `TRUE` or `FALSE` to indicate existence of container `key` in
database. Note this does not indicate if the container holds any
documents.

## Examples

``` r
if (FALSE) { # \dontrun{
src <- src_sqlite()
docdb_exists(src, "nonexistingcontainer")
docdb_create(src, "mtcars", mtcars)
docdb_exists(src, "mtcars")
} # }
```
