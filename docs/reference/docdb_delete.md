# Delete documents or container

Delete documents or container

## Usage

``` r
docdb_delete(src, key, ...)
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

  Optionally, specify `query` parameter with a JSON string as per
  [`docdb_query()`](https://docs.ropensci.org/nodbi/reference/docdb_query.md)
  to identify documents to be deleted. If not specified (default),
  deletes the container `key`.

## Value

(logical) Success of operation. Typically `TRUE` if document(s) or
collection existed, and `FALSE` if document(s) did not exist, or
collection did not exist, or delete was not successful.

## Examples

``` r
if (FALSE) { # \dontrun{
src <- src_sqlite()
docdb_create(src, "iris", iris)
docdb_delete(src, "iris", query = '{"Species": {"$regex": "a$"}}')
docdb_delete(src, "iris")
} # }
```
