# Get documents or parts with filtering query

Complements the databases' native query and filtering functions by using
[`jqr::jqr()`](https://docs.ropensci.org/jqr/reference/jqr.html). If
`query = "{}"` and neither `fields` nor `listfields` is specified, runs
[`docdb_get()`](https://docs.ropensci.org/nodbi/reference/docdb_get.md).

## Usage

``` r
docdb_query(src, key, query, ...)
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

- query:

  (character) A JSON query string, see examples. Can use comparisons /
  tests (`$lt`, `$lte`, `$gt`, `$gte`, `$ne`, `$in`, `$regex`), with
  logic operators (`$and`, `$or`, `(`, `)`), including nested queries,
  see examples. `$regex` is case-sensitive. Note that the query should
  target a field that holds a scalar or an array of scalars, not more
  complex objects.

- ...:

  Optional parameters:

  - Specify `fields` as a JSON string of fields to be returned from
    anywhere in the tree, or to be excluded from being returned, e.g.
    `fields = '{"nameOfMy.SubFieldToInclude:" 1, "_id": 0}'` and see
    examples. If `fields` is not specified, the complete JSON document
    is returned. For
    [`src_postgres()`](https://docs.ropensci.org/nodbi/reference/src_postgres.md),
    only fewer than 50 fields can be requested to be returned by the
    function.

  - Specify `limit` (integer) for the maximum number of documents to be
    returned. If `NULL` or not set (default), 10,000 for Elasticsearch
    and all documents for MongoDB, SQLite, CouchDB, PostgreSQL, and
    DuckDB.

  - Specify `listfields = TRUE` to return just the names of all fields,
    from all documents or from the maximum number of documents as
    specified in `limit`.

## Value

Data frame with requested documents, one per row, may have nested lists
in columns; `NULL` if no documents could be found. If `listfields` is
specified: vector of all field names in dot path notation.

## Note

A dot in `query` or `fields` is interpreted as a dot path, pointing to a
field nested within another, e.g. `friends.id` in the example.

## Examples

``` r
if (FALSE) { # \dontrun{
src <- src_sqlite()

docdb_create(src, "myKey", mtcars)
docdb_create(src, "myKey", contacts)
docdb_create(src, "myKey", mapdata)

docdb_query(src, "myKey", query = '{"mpg":21}')
docdb_query(src, "myKey", query = '{"mpg":21, "gear": {"$lte": 4}}')
docdb_query(src, "myKey", query = '{"mpg":21}', fields = '{"_id":0, "mpg":1, "cyl":1}')
docdb_query(src, "myKey", query = '{"_id": {"$regex": "^.+0.*$"}}', fields = '{"gear": 1}')

docdb_query(src, "myKey", query = '{"$and": [{"mpg": {"$lte": 18}}, {"gear": {"$gt": 3}}]}')
docdb_query(src, "myKey", query = '{}', fields = '{"_id":0, "mpg":1, "cyl":1}')

docdb_query(src, "myKey", query = '{"$and": [{"age": {"$gt": 21}},
 {"friends.name": {"$regex": "^B[a-z]{3,9}.*"}}]}')
docdb_query(src, "myKey", query = '{"$or": [{"rows.elements.status": "OK"}, {"$and": [
 {"_id": "5cd6785325ce3a94dfc54096"}, {"friends.name": {"$regex": "^B[a-z]{3,90}.*"}}]}]}')
docdb_query(src, "myKey", query = '{"$and": [{"_id": "5cd6785325ce3a94dfc54096"},
 {"friends.name": {"$regex": "^B[a-z]{3,90}.*"}}]}')
docdb_query(src, "myKey", query = '{"origin_addresses": {"$in": ["Santa Barbara, CA, USA",
 "New York, NY, USA"]}}', fields = '{"age": 1, "friends.id": 1, "_id": 0,
 "rows.elements.status": 1}')

docdb_query(src, "myKey", query = '{"rows.elements.status": "OK"}', listfields = TRUE)

} # }
```
