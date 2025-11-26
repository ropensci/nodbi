# Setup database connections

There is a `src_*()` function to setup a connection to each of the
database backends. The backends may have specific parameters in the
respective function `src_*()`, but all other `nodbi` functions are
independent of the backend (e.g., see
[`docdb_query()`](https://docs.ropensci.org/nodbi/reference/docdb_query.md)).

## Details

- MongoDB -
  [`src_mongo()`](https://docs.ropensci.org/nodbi/reference/src_mongo.md)

- SQLite -
  [`src_sqlite()`](https://docs.ropensci.org/nodbi/reference/src_sqlite.md)

- Elasticsearch -
  [`src_elastic()`](https://docs.ropensci.org/nodbi/reference/src_elastic.md)

- CouchDB -
  [`src_couchdb()`](https://docs.ropensci.org/nodbi/reference/src_couchdb.md)

- PostgreSQL -
  [`src_postgres()`](https://docs.ropensci.org/nodbi/reference/src_postgres.md)

- DuckDB -
  [`src_duckdb()`](https://docs.ropensci.org/nodbi/reference/src_duckdb.md)

Documentation details for each database:

- MongoDB - <https://www.mongodb.com/docs/manual/crud/>

- SQLite/JSON1 - <https://www.sqlite.org/json1.html>

- Elasticsearch - <https://www.elastic.co/docs/get-started>

- CouchDB - <https://docs.couchdb.org/en/stable/api/index.html>

- PostgreSQL -
  <https://www.postgresql.org/docs/current/functions-json.html>

- DuckDB - <https://duckdb.org/docs/stable/data/json/overview.html>

Documentation of R packages used by `nodbi` for the databases:

- mongolite - <https://CRAN.R-project.org/package=mongolite>

- RSQLite - <https://CRAN.R-project.org/package=RSQLite>

- elastic - <https://CRAN.R-project.org/package=elastic>

- sofa - <https://CRAN.R-project.org/package=sofa>

- RPostgres - <https://CRAN.R-project.org/package=RPostgres>

- duckdb - <https://CRAN.R-project.org/package=duckdb>
