# Get all documents from container in database

Get all documents from container in database

## Usage

``` r
docdb_get(src, key, limit = NULL, ...)
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

- limit:

  (integer) Maximum number of documents to be returned. If `NULL` or not
  set (default), 10,000 for Elasticsearch and all documents for MongoDB,
  SQLite, CouchDB, PostgreSQL, and DuckDB.

- ...:

  Passed on to functions:

  - MongoDB: find() in
    [`mongolite::mongo()`](https://jeroen.r-universe.dev/mongolite/reference/mongo.html)

  - SQLite: ignored

  - Elasticsearch:
    [`elastic::Search()`](https://docs.ropensci.org/elastic/reference/Search.html)

  - CouchDB:
    [`sofa::db_alldocs()`](https://docs.ropensci.org/sofa/reference/db_alldocs.html)

  - PostgreSQL: ignored

  - DuckDB: ignored

## Value

Data frame, one document per row

## Examples

``` r
if (FALSE) { # \dontrun{
src <- src_sqlite()
docdb_create(src, "mtcars", mtcars)
docdb_get(src, "mtcars", limit = 10L)
} # }
```
