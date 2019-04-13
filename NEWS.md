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
