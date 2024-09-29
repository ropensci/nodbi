# nodbi 0.10.7

## Changes
* uses new features of `duckdb` 1.11.0 for refactoring of `docdb_query()`, accelerating queries
* accelerated creating and updating from file

# nodbi 0.10.6

## Changes
* partial refactoring of `docdb_query()`, accelerating queries up to 20-fold for SQLite, DuckDB, and accelerating `listfields = TRUE` several times for DuckDB

## Bug fix
* address `docdb_query()` not working for cases when dot paths had no counts between fields
* address wrong database size printing

# nodbi 0.10.5

## Bug fix
* stop if query is invalid even though JSON is valid 
* print information also for MongoDB connection object

## Changes
* code cleaning, parameters checking
* document that `$regex` in `docdb_query()` is case-sensitive

# nodbi 0.10.4

## Bug fix
* re-adding field formatting for `docdb_query(src, key, query, listfields = TRUE, limit = <integer>)`

# nodbi 0.10.3

## Bug fix
* minor fixes to `limit` in `docdb_query(src, key, query, listfields = TRUE, limit = <integer>)` and speed up

# nodbi 0.10.2

## Changes
* added vignette
* added tests internal functions, verbose option
* added caching to GitHub action workflow
* added missing fields validity check for duckdb
* more robust parameter checks in `docdb_query` and `docdb_update`
* ensure `NULL` also for all MongoDB returns

## Bug fixes
* docTyp'ed src.R
* minify `JSON` with Elasticsearch in `docdb_update`
* moved local variable out of UseMethod in `docdb_query`

# nodbi 0.10.1

## Bug fix
* make `docdb_get()` work again for `src_sqlite()` by casting `JSONB` back to `JSON`

# nodbi 0.10.0

## Deprecated
* empty parameter `query` now triggers a warning as it should be a valid JSON string; change `query = ""` into `query = "{}"` 

## Changes
* adapted to use new, faster `JSONB` functions in `SQLite` 3.45.0 (`RSQLite` >= 2.3.4.9005)
* refactored parts of `docdb_create()` to speed up handling large data frames and lists
* made Elasticsearch to immediately refresh index after `docdb_create()` and other functions
* `docdb_update()` now reports which records failed to update and then continues
* `docdb_delete()` now returns harmonised success logical value across backends

## Potentially breaking change
`docdb_query()` reimplementation to have the same functionality across all databases (DuckDB, SQLite, PostgreSQL, MongoDB, Elasticsearch, CouchDB); even though the API and unit tests remained, user provisions may break e.g. to handle return values of databases that previously were incompletely implemented (in particular Elasticsearch and CouchDB). Details: 

* `query` can now be complex (nested, various operators) and is digested with a Javascript helper
* `fields` can now be nested fields (e.g., `friends.name`) to directly return values lifted from the nested field 
* `listfields` parameter newly implemented to return dot paths for all fields of all or selected documents in collection 
* expanded use of `jq` via `jqr` for mangling parameters, selecting documents, filtering fields and lifting nested field values
* if no data are found, returns `NULL` (previously some backends returned an empty data frame)
* `docdb_query(src, key, query = "{}", fields = "{}")` now delegates to `docdb_get(src, key)`
* `_id` is always returned, unless specified with `"_id": 0` in parameter `fields`
* for `scr_postgres`, only fewer than 50 fields if any can be specified in `fields`
* for `src_sqlite`, minimise the use of the time-costly `json_tree`
* workaround for path collisions of MongoDB
* some acceleration of `docdb_query()`
* factored out common code 
* expanded testing
* updated docs

# nodbi 0.9.8

## Bug fixes
* escaping newline character within a JSON value, in `docdb_*()` functions

# nodbi 0.9.7

## Changes
* changed `docdb_update()` to directly use NDJSON from file for duckdb
* cleaned up unnecessary code in `docdb_create()`
* no more using transactions with `src_duckdb()`

## Bug fixes
* regression error from not specifying top-level jq script
* corrected and improve field selection in `docdb_query()`
* corrected test exceptions for mongodb, updated GitHub Actions, expanded tests

# nodbi 0.9.6

## Bug fixes
* corrected marginal case in `docdb_query.src_duckdb()`
* corrected minimum R version
* replaced in tests `httpbin` with `webfakes`
* removed explicit UTF-8 encoding reference
* speed up in `docdb_query()`
* switched to v2 GitHub r-lib/action for R CMD check

# nodbi 0.9.5

## Changes
* replaced a dependency, gained speed

# nodbi 0.9.4

## Bug fixes
* fix initialisation in `docdb_query()` with `src_duckdb()`

# nodbi 0.9.3

## Changes
* `docdb_update()` now can do bulk updates when _id's are in `value` 
  (for SQLite, DuckDB, PostgreSQL, MongoDB; not yet for CouchDB and Elastic)

## Bug fixes
* fix tests for value parameter to be a file or an url

# nodbi 0.9.2

## Bug fixes
* `src_duckdb()` handles when json_type returns NULL for non-existing path
* `src_sqlite()` handles when text includes double quotation marks

## Changes
* added warning if DuckDB's JSON extension is not available; improve instructions; see also issue #45
* minor simplification of `docdb_exists()` for `src_mongo()`, and of `docdb_query()` for SQL databases

# nodbi 0.9.1

## Changes
* corrected closing connections to SQL database backends upon session restart
* improved provisions for parallel write access and corresponding tests
* capture marginal case of no rows in `docdb_query()`

# nodbi 0.9.0

## Changes
* adding support for duckdb (R package version 0.6.0 or higher) as database backend
* suppressed warnings when checking if a string points to a file

# nodbi 0.8.1

## Changes
* replaced `isa()` as not available with R version 3.x

# nodbi 0.8.0

## Changes
* refactored `docdb_update.src_couchdb()` to use `jqr`
* adapted `docdb_create` to accept `jsonlite`, `jsonify`, `jqr` JSON
* added details to README
* testing (unset LANG, relocate open code, better cleaning up)

# nodbi 0.7.1

## Bug fixes
* fixed `docdb_query()` to account for change in SQLite 3.38.3 adding quotation of labels (closes issue #44), test added
* made `docdb_query()` work for PostgreSQL when a string used with the `$in` operator has a comma(s), test added

# nodbi 0.7.0

## Improvements
* `docdb_create()` now supports file names and http urls as argument `value` for importing data
* `docdb_create()` (and thus `docdb_update()`) now supports quantifiers (e.g., '[a-z]{2,3}') in regular expressions

## Bug fixes
* for SQLite, return `FALSE` like other backends when using `docdb_delete()` for a non-existing container (table, in the case of SQLite)
* better handle special characters and encodings under Windows

# nodbi 0.6.0

## Improvements
* full support for PostgreSQL (using jsonb)

## Bug fixes
* for SQLite add closing file references also on exit

# nodbi 0.5.1

## Bug fixes
* for SQLite under Windows ensure handling of special characters (avoiding encoding conversions with file operations that stream out / in NDJSON)

# nodbi 0.5.0

## Improvements
* identical API for `docdb_*()` functions so that `query` and `fields` parameters can be used across database backends
* identical return values across database backends
* re-factored recently added functions for RSQLite
* re-factored most functions to provide identical API
* performance (timing and memory use) profiled and optimised as far as possible
* testing now uses the same test file across databases
* currently, no more support for redis (no way was found to query and update specific documents in a container)
* `docdb_list()` added as function to list container in database

## Notes
* Support for complex queries not yet implemented for Elasticsearch
* Only root fields (no subitems) returned by Elasticsearch and CouchDB

# nodbi 0.4.4

## Improvements
* made remaining `docdb_*()` functions return a logical indicating the success of the function (`docdb_create`, `docdb_delete`), or a data frame (`docdb_get`, `docdb_query`), or the number of documents affected by the function (`docdb_update`)
* change testing approach
  
## Bug fixes
* `docdb_get()` to not return '_id' field for `src_{sqlite,mongo}` since already used for row names

# nodbi 0.4.3

## Improvements
* `docdb_query.src_sqlite()` now handles JSON objects, returning nested lists (#40)
* `src_sqlite()` now uses transactions for relevant functions (#39)
* `docdb_update.src_mongo()` now returns the number of upserted or matched documents, irrespective of whether
  they were updated or not
  
## Bug fixes
* `docdb_get()` to not return '_id' field for `src_{sqlite,mongo}` since already used for row names

## Other
* change of maintainer agreed

# nodbi 0.4.2

## Bug fixes

* fix for `src_couchdb()`: we were not setting user and password correctly internally, was causing issues in CouchDB v3 (#35) thanks to @drtagkim for the pull request

# nodbi 0.4.0

## Improvements

* in `docdb_query()` and `docdb_get()`, for sqlite source, use a connection instead of a regular file path to avoid certain errors on Windows (#33) work by @rfhb
* in `docdb_query()` and `docdb_create()` for sqlite source, fix to handle mixed values of different types (#34) work by @rfhb
* some Sys.sleep's added to Elasticserch eg's to make sure data is available after creation, and before a data request

# nodbi 0.3.0

## New features

* new author Ralf Herold, with contribution of new functions for working with SQLite/json1. new functions: `src_sqlite`, `print.src_sqlite`, `docdb_create.src_sqlite`, `docdb_delete.src_sqlite`, `docdb_exists.src_sqlite`, `docdb_get.src_sqlite`, `docdb_query.src_sqlite`, and `docdb_update.src_sqlite`. includes new dataset `contacts` (#25) (#27) (#28) (#29) (#30) (#31)
* `docdb_update` gains method for working with MongoDB, via (#27)

## Improvements

* added `.github` files in the source repository to facilitate contributions
* `src_mongo` changes, improved behavior, via (#27)

## Defunct

* `etcd` (via the `etseed` package) integration has been removed from this package as etcd doesn't really fit the main goal of the pkg. functions now defunct are: `src_etcd`, `docdb_create.src_etcd`, `docdb_delete.src_etcd`, `docdb_exists.src_etcd`, `docdb_get.src_etcd`, and `print.src_etcd` (#26)

# nodbi 0.2.0

## New features

* `docdb_get()` gains `limit` parameter to do pagination, for CouchDB, 
Elasticsearch and MongoDB only (#17) (#23)
* gains function `docdb_query()` to send queries to each backend (#18) (#22)
* gains function `docdb_exists()` to check if a database or equivalent exists (#21) (#22)

## Improvements

* Updated package for new version of `elastic`, which has slightly different
setup for connecting to the Elasticsearch instance (#20)

# nodbi 0.1.0

## New features

* released to CRAN
