nodbi 0.4.2.9000
===========

### MINOR IMPROVEMENTS
* `docdb_query()` now handles JSON objects for src_sqlite (#40)
* src_sqlite now uses transactions for relevant functions (#39)

### BUG FIXES
* `docdb_get()` to not return '_id' field for src_{sqlite,mongo}

nodbi 0.4.2
===========

### BUG FIXES

* fix for `src_couchdb()`: we were not setting user and password correctly internally, was causing issues in CouchDB v3 (#35) thanks to @drtagkim for the pull request

nodbi 0.4.0
===========

### MINOR IMPROVEMENTS

* in `docdb_query()` and `docdb_get()`, for sqlite source, use a connection instead of a regular file path to avoid certain errors on Windows (#33) work by @rfhb
* in `docdb_query()` and `docdb_create()` for sqlite source, fix to handle mixed values of different types (#34) work by @rfhb
* some Sys.sleep's added to Elasticserch eg's to make sure data is available after creation, and before a data request

nodbi 0.3.0
===========

### NEW FEATURES

* new author Ralf Herold, with contribution of new functions for working with SQLite/json1. new functions: `src_sqlite`, `print.src_sqlite`, `docdb_create.src_sqlite`, `docdb_delete.src_sqlite`, `docdb_exists.src_sqlite`, `docdb_get.src_sqlite`, `docdb_query.src_sqlite`, and `docdb_update.src_sqlite`. includes new dataset `contacts` (#25) (#27) (#28) (#29) (#30) (#31)
* `docdb_update` gains method for working with MongoDB, via (#27)

### MINOR IMPROVEMENTS

* added `.github` files in the source repository to facilitate contributions
* `src_mongo` changes, improved behavior, via (#27)

### DEFUNCT

* `etcd` (via the `etseed` package) integration has been removed from this package as etcd doesn't really fit the main goal of the pkg. functions now defunct are: `src_etcd`, `docdb_create.src_etcd`, `docdb_delete.src_etcd`, `docdb_exists.src_etcd`, `docdb_get.src_etcd`, and `print.src_etcd` (#26)


nodbi 0.2.0
===========

### NEW FEATURES

* `docdb_get()` gains `limit` parameter to do pagination, for CouchDB, 
Elasticsearch and MongoDB only (#17) (#23)
* gains function `docdb_query()` to send queries to each backend (#18) (#22)
* gains function `docdb_exists()` to check if a database or equivalent exists (#21) (#22)

### MINOR IMPROVEMENTS

* Updated package for new version of `elastic`, which has slightly different
setup for connecting to the Elasticsearch instance (#20)


nodbi 0.1.0
===========

### NEW FEATURES

* released to CRAN
