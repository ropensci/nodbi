# Create documents in a database

A message is emitted if the container `key` already exists.

## Usage

``` r
docdb_create(src, key, value, ...)
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

- value:

  The data to be created in the database: a single data.frame, a JSON
  string, a list, or a file name or URL that points to NDJSON documents

- ...:

  Passed to functions
  [`sofa::db_bulk_create()`](https://docs.ropensci.org/sofa/reference/db_bulk_create.html),
  [`elastic::docs_bulk()`](https://rdrr.io/pkg/elastic/man/docs_bulk.html),
  and
  [`mongolite::mongo()`](https://jeroen.r-universe.dev/mongolite/reference/mongo.html)\$insert()

## Value

(integer) Number of successfully created documents

## Details

An error is raised for document(s) in `value` when their `_id` already
exist(s) in the collection `key`; use
[`docdb_update()`](https://docs.ropensci.org/nodbi/reference/docdb_update.md)
to update such document(s).

## Identifiers

If `value` is a data.frame that has a column `_id`, or is a JSON string
having a key `_id` at root level, or is a list having an item `_id` at
its top level, this will be used as `_id`'s and primary index in the
database. If there are no such `_id`'s in `value`, row names (if any
exist) of `value` will be used as `_id`'s, otherwise random `_id`'s will
be created (using
[`uuid::UUIDgenerate()`](https://rdrr.io/pkg/uuid/man/UUIDgenerate.html)
with `use.time = TRUE` for SQLite and PostgreSQL, or using DuckDB's
built-in `uuid()`).

## Examples

``` r
if (FALSE) { # \dontrun{
src <- src_sqlite()
docdb_create(src,
  key = "diamonds_small",
  value = as.data.frame(diamonds[1:3000L, ])
)
head(docdb_get(src, "diamonds_small"))
docdb_create(src, key = "contacts", value = contacts)
docdb_get(src, "contacts")[["friends"]]
} # }
```
