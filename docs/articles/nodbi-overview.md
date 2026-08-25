# nodbi - package overview

## Summary

R package `nodbi` provides a single interface for several NoSQL
databases and SQL databases with JSON functionality, with the same
function parameters and return values across all of:

- SQLite
- DuckDB
- PostgreSQL
- MongoDB
- CouchDB
- Elasticsearch
- MariaDB

Package `nodbi` has been designed to use any specific SQL functions a
database may have for `JSON` and has added functionality tested for
performance to enable switching databases without changing user code.

[`library`](https://rdrr.io/r/base/library.html)`(`[`nodbi`](https://docs.ropensci.org/nodbi/)`)`

## Connect

First, a connection to a database is opened. In the example, no
additional parameters are used such as database file or server; see the
help page for the respective database.

“Container” is used as term to indicate where conceptually the database
holds the data (e.g. a collection in MongoDB, a table in DuckDB). The
`key` parameter of `nodbi` functions holds the name of the relevant
container.

`# name of container`` ``key`` ``<-`` ``"my_container"`` `` ``# nodbi can connect any of these databases`` ``if`` ``(``FALSE``)`` ``{`` `` ``src`` ``<-`` `[`src_duckdb`](https://docs.ropensci.org/nodbi/reference/src_duckdb.md)`(``)`` `` ``src`` ``<-`` `[`src_mongo`](https://docs.ropensci.org/nodbi/reference/src_mongo.md)`(``collection ``=`` ``key``)`` `` ``src`` ``<-`` `[`src_sqlite`](https://docs.ropensci.org/nodbi/reference/src_sqlite.md)`(``)`` `` ``src`` ``<-`` `[`src_postgres`](https://docs.ropensci.org/nodbi/reference/src_postgres.md)`(``)`` `` ``src`` ``<-`` `[`src_mariadb`](https://docs.ropensci.org/nodbi/reference/src_mariadb.md)`(``)`` `` ``src`` ``<-`` `[`src_elastic`](https://docs.ropensci.org/nodbi/reference/src_elastic.md)`(``)`` `` ``src`` ``<-`` `[`src_couchdb`](https://docs.ropensci.org/nodbi/reference/src_couchdb.md)`(`` `` user ``=`` `[`Sys.getenv`](https://rdrr.io/r/base/Sys.getenv.html)`(``"COUCHDB_TEST_USER"``)``,`` `` pwd ``=`` `[`Sys.getenv`](https://rdrr.io/r/base/Sys.getenv.html)`(``"COUCHDB_TEST_PWD"``)`` `` ``)`` ``}`` `` ``# this example is run with`` ``src`` ``<-`` `[`src_sqlite`](https://docs.ropensci.org/nodbi/reference/src_sqlite.md)`(``)`` ``#> Warning: Database is only in memory, will not persist after R ends! Consider copying it with `` ``#> RSQLite::sqliteCopyDatabase(`` ``#> from = <your nodbi::src_sqlite() object>$con, `` ``#> to = <e.g. RSQLite::dbConnect(RSQLite::SQLite(), 'local_file.sqlite')>`` ``#> )`` `` ``# note additional parameters can be specified,`` ``# for example for local or remote MongoDb:`` `[`help`](https://rdrr.io/r/utils/help.html)`(``"src_mongo"``)`

## docdb_create

Create a container if it does not yet exist and fill with `value`. The
return value is the number of created documents. “Documents” refers to
the rows in a data frame such as `mtcars`, or the number of `NDJSON`
lines, or the number of list items, or the number of objects in an
`JSON` array.

The parameter `value` in any `nodbi` function can take a data frame, a
list, a JSON string, or a file name or URL pointing to `NDJSON`.

`# check if container already exists`` `[`docdb_exists`](https://docs.ropensci.org/nodbi/reference/docdb_exists.md)`(``src ``=`` ``src``, key ``=`` ``key``)`` ``#> [1] FALSE`` `` ``# load data from a data frame with row names into`` ``# the container specified in "key" parameter`` `[`docdb_create`](https://docs.ropensci.org/nodbi/reference/docdb_create.md)`(``src ``=`` ``src``, key ``=`` ``key``, value ``=`` ``mtcars``)`` ``#> [1] 32`` `` ``# do not run during testing`` ``if`` ``(``FALSE``)`` ``{`` `` ``# load additionally 98 NDJSON records`` `` `[`docdb_create`](https://docs.ropensci.org/nodbi/reference/docdb_create.md)`(``src``, ``key``, ``"https://httpbin.org/stream/98"``)`` ``}`` `` ``# load additionally mapdata as list`` `[`docdb_create`](https://docs.ropensci.org/nodbi/reference/docdb_create.md)`(``src``, ``key``, ``jsonlite``::`[`fromJSON`](https://jeroen.r-universe.dev/jsonlite/reference/fromJSON.html)`(``mapdata``, simplifyVector ``=`` ``FALSE``)``)`` ``#> Note: container 'my_container' already exists`` ``#> [1] 2`` `` ``# show JSON structure of contacts`` ``jsonlite``::`[`minify`](https://jeroen.r-universe.dev/jsonlite/reference/prettify.html)`(``contacts``)`` ``#> [{"_id":"5cd67853f841025e65ce0ce2","isActive":false,"balance":"$3,808.45","age":23,"eyeColor":"green","name":"Lacy Chen","email":"lacychen@conjurica.com","about":"Sunt consequat ad dolore.\nExercitation nisi reprehenderit.","registered":"2014-08-03T12:11:54 -02:00","tags":["nulla","nisi","adipisicing","do","ad","ullamco","irure"],"friends":[{"id":0,"name":"Wooten Goodwin"},{"id":1,"name":"Brandie Woodward"},{"id":2,"name":"Angelique Britt"}]},{"_id":"5cd678531b423d5f04cfb0a1","isActive":false,"balance":"$3,400.50","age":20,"eyeColor":"brown","name":"Rae Colon","email":"raecolon@conjurica.com","about":"Nisi excepteur duis duis aliquip qui id consequat consequat.","registered":"2018-12-19T06:23:35 -01:00","tags":["nostrud","eu","consectetur","adipisicing","labore","ut","voluptate"],"friends":[{"id":0,"name":"Yang Yates"},{"id":1,"name":"Lacy Chen"}]},{"_id":"5cd6785335b63cb19dfa8347","isActive":false,"balance":"$2,579.09","age":30,"eyeColor":"brown","name":"Williamson French","email":"williamsonfrench@conjurica.com","about":"Nulla do sunt consectetur officia. Laboris pariatur incididunt.","registered":"2018-02-14T10:59:57 -01:00","tags":["exercitation","do","magna","ut","consectetur","ex","incididunt"],"friends":[{"id":0,"name":"Coleen Dunn"},{"id":1,"name":"Doris Phillips"},{"id":2,"name":"Concetta Turner"}]},{"_id":"5cd6785325ce3a94dfc54096","isActive":true,"balance":"$1,161.52","age":22,"eyeColor":"brown","name":"Pace Bell","email":"pacebell@conjurica.com","about":"Eiusmod sunt laborum ipsum do cupidatat qui id dolore do.","registered":"2018-08-17T12:23:42 -02:00","tags":["aliqua","consectetur","commodo","velit","cupidatat","duis","dolore"],"friends":[{"id":0,"name":"Baird Keller"},{"id":1,"name":"Francesca Reese"},{"id":2,"name":"Dona Bartlett"}]},{"_id":"5cd678530df22d3625ed8375","isActive":true,"balance":"$2,412.67","age":20,"eyeColor":"blue","name":"Krista Baxter","email":"kristabaxter@conjurica.com","about":"Sint quis nulla ea fugiat. Commodo nisi qui eu sit.","registered":"2017-07-19T05:03:47 -02:00","tags":["sit","cillum","commodo","labore","sint","in","exercitation"],"friends":[{"id":0,"name":"Pace Bell"}]}]`` `` ``# load additionally contacts JSON data`` `[`docdb_create`](https://docs.ropensci.org/nodbi/reference/docdb_create.md)`(``src``, ``key``, ``contacts``)`` ``#> Note: container 'my_container' already exists`` ``#> [1] 5`

Check and list any other containers exist in the database:

[`docdb_list`](https://docs.ropensci.org/nodbi/reference/docdb_list.md)`(``src ``=`` ``src``)`` ``#> [1] "my_container"`

## Identifiers

The unique document identifier is its `_id`, corresponding to a primary
index with a constraint to be unique in SQL databases.

The `_id`’s of an input `value` are either the row names of a data frame
(such as `mtcars`) or top-level elements with the name `_id` such as in
`contacts` shown just above.

Thus, expect a warning when trying to create documents with `_id`’s that
already exist in the container.

The return value can be `0` when no documents could newly be created, or
the number of the subset of documents in `value` that did not yet exist
and could newly be created.

`# zero new documents created`` `[`docdb_create`](https://docs.ropensci.org/nodbi/reference/docdb_create.md)`(``src``, ``key``, value ``=`` ``mtcars``)`` ``#> Note: container 'my_container' already exists`` ``#> Warning: Could not create some documents, reason: UNIQUE constraint failed`` ``#> [1] 0`

For updating existing documents, see below function
[`docdb_update()`](https://docs.ropensci.org/nodbi/reference/docdb_update.md).

## docdb_get

All documents in a container can now be retrieved with
[`docdb_get()`](https://docs.ropensci.org/nodbi/reference/docdb_get.md).

`# use library for more`` ``# readable print output`` ``if`` ``(`[`require`](https://rdrr.io/r/base/library.html)`(`[`tibble`](https://tibble.tidyverse.org/)`)``)`` ``{`` `` `` `` ``# get all documents, irrespective of schema`` `` `[`as_tibble`](https://tibble.tidyverse.org/reference/as_tibble.html)`(`[`docdb_get`](https://docs.ropensci.org/nodbi/reference/docdb_get.md)`(``src``, ``key``)``)`` `` `` `` ``# get just 2 documents using limit and note that`` `` ``# only fields for these documents are returned`` `` `[`as_tibble`](https://tibble.tidyverse.org/reference/as_tibble.html)`(`[`docdb_get`](https://docs.ropensci.org/nodbi/reference/docdb_get.md)`(``src``, ``key``, limit ``=`` ``2L``)``)`` `` `` ``}`` ``#> Loading required package: tibble`` ``#> ``# A tibble: 2 × 5`` ``` #> `_id` destination_addresses origin_addresses rows status ``` ``#> ``<chr>`` ``<list>`` ``<list>`` ``<lis>`` ``<chr>`` `` ``#> ``1`` 15f694e6-a099-11f1-8b13-6… ``<chr [3]>`` ``<chr [2]>`` ``<df>`` OK `` ``#> ``2`` 15f694f0-a099-11f1-8b13-6… ``<chr [3]>`` ``<chr [1]>`` ``<df>`` OK`

## docdb_query

One of the most powerful functions of `nodbi` is
[`docdb_query()`](https://docs.ropensci.org/nodbi/reference/docdb_query.md)
because it permits to combine a query to select documents and to filter
for fields of interest.

`# query for some documents`` `[`docdb_query`](https://docs.ropensci.org/nodbi/reference/docdb_query.md)`(``src ``=`` ``src``, key ``=`` ``key``, query ``=`` ``'{"mpg": {"$gte": 30}}'``)`` ``#> _id mpg cyl disp hp drat wt qsec vs am gear carb`` ``#> 1 Fiat 128 32.4 4 78.7 66 4.08 2.200 19.47 1 1 4 1`` ``#> 2 Honda Civic 30.4 4 75.7 52 4.93 1.615 18.52 1 1 4 2`` ``#> 3 Toyota Corolla 33.9 4 71.1 65 4.22 1.835 19.90 1 1 4 1`` ``#> 4 Lotus Europa 30.4 4 95.1 113 3.77 1.513 16.90 1 1 5 2`

Both parameters `query` (obligatory) and `fields` (optional) use, across
all databases, MongoDB syntax such as documented for
[queries](https://www.mongodb.com/docs/manual/crud/) and
[fields](https://www.mongodb.com/docs/manual/tutorial/project-fields-from-query-results/).

`# query some fields from some documents; 'query' is a mandatory`` ``# parameter and is used here in its position in the signature`` `[`docdb_query`](https://docs.ropensci.org/nodbi/reference/docdb_query.md)`(``src``, ``key``, ``'{"mpg": {"$gte": 30}}'``, fields ``=`` ``'{"wt": 1, "mpg": 1}'``)`` ``#> _id wt mpg`` ``#> 1 Fiat 128 2.200 32.4`` ``#> 2 Honda Civic 1.615 30.4`` ``#> 3 Toyota Corolla 1.835 33.9`` ``#> 4 Lotus Europa 1.513 30.4`

Unless `fields` specifies `"_id": 0`, the `_id` field is always included
in the output of
[`docdb_query()`](https://docs.ropensci.org/nodbi/reference/docdb_query.md).

`# query some fields from some documents, limit return to one document`` `[`docdb_query`](https://docs.ropensci.org/nodbi/reference/docdb_query.md)`(``src``, ``key``, ``'{"mpg": {"$gte": 30}}'``, fields ``=`` ``'{"_id": 0, "mpg": 1}'``, limit ``=`` ``1L``)`` ``#> mpg`` ``#> 1 32.4`

Queries can be more complex such as in this example, showing a dot
notation of a sub-field and an example operator (regular expression).

`# query some subitem fields from some documents`` `[`str`](https://rdrr.io/r/utils/str.html)`(`[`docdb_query`](https://docs.ropensci.org/nodbi/reference/docdb_query.md)`(`` `` ``src``, ``key``,`` `` query ``=`` ``'{"$or": [{"age": {"$gt": 21}},`` `` {"friends.name": {"$regex": "^B[a-z]{3,9}.*"}}]}'``,`` `` fields ``=`` ``'{"age": 1, "friends.name": 1}'`` ``)``)`` ``#> 'data.frame': 3 obs. of 3 variables:`` ``#> $ _id : chr "5cd67853f841025e65ce0ce2" "5cd6785335b63cb19dfa8347" "5cd6785325ce3a94dfc54096"`` ``#> $ age : int 23 30 22`` ``#> $ friends.name:List of 3`` ``#> ..$ : chr "Wooten Goodwin" "Brandie Woodward" "Angelique Britt"`` ``#> ..$ : chr "Coleen Dunn" "Doris Phillips" "Concetta Turner"`` ``#> ..$ : chr "Baird Keller" "Francesca Reese" "Dona Bartlett"`

Queries work across documents of different structure such as here.

`# query with results across documents`` `[`docdb_query`](https://docs.ropensci.org/nodbi/reference/docdb_query.md)`(`` `` ``src``, ``key``,`` `` query ``=`` ``'{"$or": [{"age": {"$gt": 21}}, {"mpg": {"$gte": 30}}]}'``,`` `` fields ``=`` ``'{"name": 1, "disp": 1}'`` ``)`` ``#> _id name disp`` ``#> 1 Fiat 128 <NA> 78.7`` ``#> 2 Honda Civic <NA> 75.7`` ``#> 3 Toyota Corolla <NA> 71.1`` ``#> 4 Lotus Europa <NA> 95.1`` ``#> 5 5cd67853f841025e65ce0ce2 Lacy Chen NA`` ``#> 6 5cd6785335b63cb19dfa8347 Williamson French NA`` ``#> 7 5cd6785325ce3a94dfc54096 Pace Bell NA`

## Field names

The `JSON` data handled by package `nodbi` may have a large number of
field included nested fields in objects (see for example `name` within
array `friends` above). Thus, an argument is provided for
[`docdb_query()`](https://docs.ropensci.org/nodbi/reference/docdb_query.md)
so that the function returns only the comprehensive list of all field
names in documents selected with a query (or in all documents in the
container if `query = "{}"` is specified).

[`docdb_query`](https://docs.ropensci.org/nodbi/reference/docdb_query.md)`(``src``, ``key``, query ``=`` ``'{"_id": {"$regex": "^[0-9]"}}'``, listfields ``=`` ``TRUE``)`` ``#> [1] "about" "age" `` ``#> [3] "balance" "destination_addresses" `` ``#> [5] "email" "eyeColor" `` ``#> [7] "friends" "friends.id" `` ``#> [9] "friends.name" "isActive" `` ``#> [11] "name" "origin_addresses" `` ``#> [13] "registered" "rows" `` ``#> [15] "rows.elements" "rows.elements.distance" `` ``#> [17] "rows.elements.distance.somevalue" "rows.elements.distance.text" `` ``#> [19] "rows.elements.duration" "rows.elements.duration.somevalue"`` ``#> [21] "rows.elements.duration.text" "rows.elements.status" `` ``#> [23] "status" "tags"`

The dot notation is a path from a root field to the nested field, and
this notation can be used in `query` and `fields` parameters of
[`docdb_query()`](https://docs.ropensci.org/nodbi/reference/docdb_query.md).

## docdb_update

Queries can also be used for updating (patching) selected documents with
a new `value`. The return value of
[`docdb_update()`](https://docs.ropensci.org/nodbi/reference/docdb_update.md)
corresponds to the number of documents that were updated.

This is another powerful function because `value` can come from a data
frame, a list, a JSON string, or a file name or URL pointing to
`NDJSON`, and if `value` includes row names or `_id`’s, these are used
to identify the documents to be updated.

`# number of documents corresponding to query`` `[`nrow`](https://rdrr.io/r/base/nrow.html)`(`[`docdb_query`](https://docs.ropensci.org/nodbi/reference/docdb_query.md)`(``src``, ``key``, query ``=`` ``'{"carb": 3}'``)``)`` ``#> [1] 3`` `` ``# update all documents using JSON, replacing the previously existing values`` `[`docdb_update`](https://docs.ropensci.org/nodbi/reference/docdb_update.md)`(``src``, ``key``, value ``=`` ``'{"vs": 9, "xy": [1, 2]}'``, query ``=`` ``'{"carb": 3}'``)`` ``#> [1] 3`` `` ``# update with value that includes _id's`` `[`docdb_update`](https://docs.ropensci.org/nodbi/reference/docdb_update.md)`(``src``, ``key``, value ``=`` ``'{"_id": "Merc 450SLC", "xy": 33}'``, query ``=`` ``"{}"``)`` ``#> [1] 1`` `` ``# show updated values`` `[`docdb_query`](https://docs.ropensci.org/nodbi/reference/docdb_query.md)`(``src``, ``key``, query ``=`` ``'{"carb": 3}'``, fields ``=`` ``'{"xy": 1}'``)`` ``#> _id xy`` ``#> 1 Merc 450SE 1, 2`` ``#> 2 Merc 450SL 1, 2`` ``#> 3 Merc 450SLC 33`

## docdb_delete

Documents and containers can be deleted with
[`docdb_delete()`](https://docs.ropensci.org/nodbi/reference/docdb_delete.md).
Its return value corresponds to the success of the delete operation.

`# number of documents corresponding to query`` `[`nrow`](https://rdrr.io/r/base/nrow.html)`(`[`docdb_query`](https://docs.ropensci.org/nodbi/reference/docdb_query.md)`(``src``, ``key``, query ``=`` ``'{"age": {"$lte": 23}}'``)``)`` ``#> [1] 4`` `` ``# to delete selected documents, specify a query parameter`` `[`docdb_delete`](https://docs.ropensci.org/nodbi/reference/docdb_delete.md)`(``src``, ``key``, query ``=`` ``'{"age": {"$lte": 23}}'``)`` ``#> [1] TRUE`` `` ``# this deletes the complete container from database`` `[`docdb_delete`](https://docs.ropensci.org/nodbi/reference/docdb_delete.md)`(``src``, ``key``)`` ``#> [1] TRUE`` `` ``# check if still exists`` `[`docdb_exists`](https://docs.ropensci.org/nodbi/reference/docdb_exists.md)`(``src``, ``key``)`` ``#> [1] FALSE`

## Disconnect and shutdown

Package `nodbi` includes an automatic mechanism for shutting down, at
the time of [`quit()`](https://rdrr.io/r/base/quit.html) or session
restart, those databases that require it (SQLite, DuckDB, PostgreSQL).

Nevertheless, it is good practice to manually disconnect and shut down
connections as specific to the database, for example for SQLite:

`src`` ``#> src: SQLite`` ``#> ver: 3.53.3`` ``#> db(s): :memory:`` ``#> size(s): 0.00338 MB`` `` ``# shutdown`` ``DBI``::`[`dbDisconnect`](https://dbi.r-dbi.org/reference/dbDisconnect.html)`(``src``$``con``, shutdown ``=`` ``TRUE``)`` `[`rm`](https://rdrr.io/r/base/rm.html)`(``src``)`
