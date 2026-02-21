# Update documents

Documents are updated by patching their JSON with `value`. Documents are
identified by a `query` or by `_id`'s in `value`, where the latter takes
precedence. `value` can have multiple documents (with `_id`'s), which
then are iteratively updated.

## Usage

``` r
docdb_update(src, key, value, query, ...)
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

- query:

  (character) A JSON query string, see examples. Can use comparisons /
  tests (`$lt`, `$lte`, `$gt`, `$gte`, `$ne`, `$in`, `$regex`), with
  logic operators (`$and`, `$or`, `(`, `)`), including nested queries,
  see examples. Specify as `'{}'` if `value` includes `_id`'s.

- ...:

  Passed on to functions
  [`elastic::docs_bulk_update()`](https://rfhb.github.io/elastic/reference/docs_bulk_update.html),
  and
  [`mongolite::mongo()`](https://jeroen.r-universe.dev/mongolite/reference/mongo.html)\$update().

## Value

(integer) Number of successfully updated documents

## Details

Uses native functions in MongoDB
([`mongolite::mongo()`](https://jeroen.r-universe.dev/mongolite/reference/mongo.html)\$update()),
SQLite (`jsonb_update()`), DuckDB (`jsonb_merge_patch()`), Elasticsearch
([`elastic::docs_bulk_update()`](https://rfhb.github.io/elastic/reference/docs_bulk_update.html));
a `plpgsql` function added when calling
[`src_postgres()`](https://docs.ropensci.org/nodbi/reference/src_postgres.md),
and a [`jqr::jqr()`](https://docs.ropensci.org/jqr/reference/jqr.html)
programme for CouchDB.

## Examples

``` r
if (FALSE) { # \dontrun{
src <- src_sqlite()
docdb_create(src, "mtcars", mtcars)
docdb_update(src, "mtcars", value = mtcars[3, 4:5], query = '{"gear": 3}')
docdb_update(src, "mtcars", value = '{"carb":999}', query = '{"gear": 5}')
docdb_update(src, "mtcars", value = '{"_id":"Fiat 128", "carb":888}', query = '{}')
docdb_get(src, "mtcars")
} # }
```
