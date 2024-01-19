
# nodbi

⁠<!-- badges: start -->
[![R-CMD-check](https://github.com/ropensci/nodbi/workflows/R-CMD-check/badge.svg)](https://github.com/ropensci/nodbi/actions?query=workflow%3AR-CMD-check)
[![codecov](https://codecov.io/gh/rfhb/nodbi/branch/master/graph/badge.svg)](https://app.codecov.io/gh/rfhb/nodbi)
[![CRAN
status](https://www.r-pkg.org/badges/version/nodbi)](https://CRAN.R-project.org/package=nodbi)
[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
⁠<!-- badges: end -->

`nodbi` is an R package that provides a single interface for several
NoSQL databases and databases with JSON functionality, with the same
function parameters and return values across all database backends. Last
updated 2024-01-18.

| Currently, `nodbi` supports<br/>as database backends | for an `R` object of any<br/>of these data types | for these operations |
|:-----------------------------------------------------|:-------------------------------------------------|:---------------------|
| MongoDB                                              | data.frame                                       | List, Exists         |
| SQLite                                               | list                                             | Create               |
| PostgreSQL                                           | JSON string                                      | Get                  |
| DuckDB                                               | file name of NDJSON records                      | Query                |
| Elasticsearch                                        | URL of NDJSON records                            | Update               |
| CouchDB                                              |                                                  | Delete               |

For speed comparisons of database backends, see [benchmark](#benchmark)
and [testing](#testing) below.

## API overview

Parameters for `docdb_*()` functions are the same across all database
backends. See [walk-through](#walk-through) below and the canonical
testing in [core-nodbi.R](./tests/testthat/core-nodbi.R). “Container” is
used as term to indicate where conceptually the backend holds the data,
see [Database connections](#database-connections) below. The `key`
parameter holds the name of a container.

| Purpose                                                                                                                                                 | Function call                                                                                                                                         |
|:--------------------------------------------------------------------------------------------------------------------------------------------------------|:------------------------------------------------------------------------------------------------------------------------------------------------------|
| Create database connection (see below)                                                                                                                  | `src <- nodbi::src_{duckdb, postgres, mongo, sqlite, couchdb, elastic}(<see below for parameters>)`                                                   |
| Load `my_data` (a data frame, list, JSON string, or file name or URL pointing to NDJSON records) into database, container `my_container`                | `nodbi::docdb_create(src = src, key = "my_container", value = my_data)`                                                                               |
| Get all documents back into a data frame                                                                                                                | `nodbi::docdb_get(src = src, key = "my_container")`                                                                                                   |
| Get documents selected with query (as MongoDB-compatible JSON) into a data frame                                                                        | `nodbi::docdb_query(src = src, key = "my_container", query = '{"age": 20}')`                                                                          |
| Get selected fields (in MongoDB compatible JSON) from documents selected by query into a data frame                                                     | `nodbi::docdb_query(src = src, key = "my_container", query = '{"age": {"$gt": 20}}', fields = '{"friends.name": 1, "_id": 0, "age": 1}', limit = 2L)` |
| Update (patch) documents selected by query with new data `my_data` (in a data frame, list, JSON string, or file name or URL pointing to NDJSON records) | `nodbi::docdb_update(src = src, key = "my_container", value = my_data, query = '{"age": 20}')`                                                        |
| Check if container exists                                                                                                                               | `nodbi::docdb_exists(src = src, key = "my_container")`                                                                                                |
| List all containers in database                                                                                                                         | `nodbi::docdb_list(src = src)`                                                                                                                        |
| Delete document(s) in container                                                                                                                         | `nodbi::docdb_delete(src = src, key = "my_container", query = '{"age": 20}')`                                                                         |
| Delete container                                                                                                                                        | `nodbi::docdb_delete(src = src, key = "my_container")`                                                                                                |
| Close and remove database connection manually (when restarting R, connections are automatically closed and removed by `nodbi`)                          | `rm(src)`                                                                                                                                             |

## Install

CRAN version

``` r
install.packages("nodbi")
```

Development version

``` r
remotes::install_github("ropensci/nodbi")
```

Load package from library

``` r
library("nodbi")
```

## Database connections

Overview on parameters and aspects that are specific to the database
backend. These are only needed once, for for `src_*()` to create a
connection object. Any such connection object is subsequently used
similarly across the `docdb_*` functions.

“Container” refers to how conceptually the backend holds the data. Data
types are mapped from JSON to R objects by
[jsonlite](https://CRAN.R-project.org/package=jsonlite). Any root-level
`_id` is extracted from the document(s) and used for an index column
`_id`, otherwise a UUID is created as `_id`.

### DuckDB

See also <https://CRAN.R-project.org/package=duckdb>. “Container” refers
to a DuckDB table, with columns `_id` and `json` created and used by
package `nodbi`, applying SQL functions and functions as per
<https://duckdb.org/docs/extensions/json> to the `json` column. Each row
in the table represents a `JSON` document.

``` r
src <- nodbi::src_duckdb(dbdir = ":memory:", ...)
```

### MongoDB

“Container” refers to a MongoDB collection, in which `nodbi` creates
JSON documents. See also <https://jeroen.github.io/mongolite/>. MongoDB
but none of the other databases require to specify the container name
already in the `src_*()` function; use the `collection` name for
parameter `key` in `docdb_*` functions.

``` r
src <- nodbi::src_mongo(
  collection = "my_container", db = "my_database",
  url = "mongodb://localhost", ...)
```

### SQLite

“Container” refers to an SQLite table, with columns `_id` and `json`
created and used by package `nodbi`, applying SQL functions and
functions as per <https://www.sqlite.org/json1.html> to the `json`
column. Each row in the table represents a `JSON` document. The table is
indexed on `_id`. See also <https://CRAN.R-project.org/package=RSQLite>.

``` r
src <- nodbi::src_sqlite(dbname = ":memory:", ...)
```

### CouchDB

“Container” refers to a CouchDB database, in which `nodbi` creates JSON
documents. See also <https://CRAN.R-project.org/package=sofa>. With
CouchDB, function `docdb_update()` uses
[jqr](https://cran.r-project.org/package=jqr) to implement patching
JSON, in analogy to functions available for the other databases.

``` r
src <- nodbi::src_couchdb(
  host = "127.0.0.1", port = 5984L, path = NULL,
  transport = "http", user = NULL, pwd = NULL, headers = NULL)
```

### Elasticsearch

“Container” refers to an Elasticsearch index, in which `nodbi` creates
JSON documents. Opensearch can equally be used. See also
<https://CRAN.R-project.org/package=elastic>. Only lowercase is accepted
for container names (in parameter `key` of `docdb_*` functions).

``` r
src <- nodbi::src_elastic(
  host = "127.0.0.1", port = 9200L, path = NULL,
  transport_schema = "http", user = NULL, pwd = NULL, ...)
```

### PostgreSQL

“Container” refers to a PostgreSQL table, with columns `_id` and `json`
created and used by package `nodbi`, applying SQL functions and
functions as per
<https://www.postgresql.org/docs/current/functions-json.html> to the
`json` column. With PostgreSQL, a custom `plpgsql` function
[jsonb_merge_patch()](https://github.com/ropensci/nodbi/blob/master/R/src_postgres.R#L60)
is used for `docdb_update()`. The order of variables in data frames
returned by `docdb_get()` and `docdb_query()` can differ from their
order the input to `docdb_create()`.

``` r
src <- nodbi::src_postgres(
  dbname = "my_database", host = "127.0.0.1", port = 5432L, ...)
```

## Walk-through

This example is to show how functional `nodbi` is at this time: With any
of the six database backends, the functions work in the same way and
return the same values.

``` r
# load nodbi
library(nodbi)

# name of container
key <- "my_container"

# connect any of these database backends
src <- src_duckdb()
src <- src_mongo(collection = key)
src <- src_sqlite()
src <- src_postgres()
src <- src_elastic()
src <- src_couchdb(
  user = Sys.getenv("COUCHDB_TEST_USER"), 
  pwd = Sys.getenv("COUCHDB_TEST_PWD"))

# check if container already exists
docdb_exists(src, key)
# [1] FALSE

# load data (here data frame, alternatively list or JSON)
# into the container "my_container" specified in "key" parameter
docdb_create(src, key, value = mtcars)
# [1] 32

# load additionally 98 NDJSON records
docdb_create(src, key, "https://httpbin.org/stream/98")
# Note: container 'my_container' already exists
# [1] 98

# load additionally contacts JSON data, from package nodbi
docdb_create(src, key, contacts)
# Note: container 'my_container' already exists
# [1] 5

# get all documents, irrespective of schema
dplyr::tibble(docdb_get(src, key))
# # A tibble: 135 × 27
#    `_id` isActive balance   age eyeColor name  email about registered tags   friends
#    <chr> <lgl>    <chr>   <int> <chr>    <chr> <chr> <chr> <chr>      <list> <list> 
#  1 5cd6… TRUE     $2,412…    20 blue     Kris… kris… "Sin… 2017-07-1… <chr>  <df>   
#  2 5cd6… FALSE    $3,400…    20 brown    Rae … raec… "Nis… 2018-12-1… <chr>  <df>   
#  3 5cd6… TRUE     $1,161…    22 brown    Pace… pace… "Eiu… 2018-08-1… <chr>  <df>   
#  4 5cd6… FALSE    $2,579…    30 brown    Will… will… "Nul… 2018-02-1… <chr>  <df>   
#  5 5cd6… FALSE    $3,808…    23 green    Lacy… lacy… "Sun… 2014-08-0… <chr>  <df>   
#  6 69bc… NA       NA         NA NA       NA    NA     NA   NA         <NULL> <NULL> 
#  7 69bc… NA       NA         NA NA       NA    NA     NA   NA         <NULL> <NULL> 
#  8 69bc… NA       NA         NA NA       NA    NA     NA   NA         <NULL> <NULL> 
#  9 69bc… NA       NA         NA NA       NA    NA     NA   NA         <NULL> <NULL> 
# 10 69bc… NA       NA         NA NA       NA    NA     NA   NA         <NULL> <NULL> 
# # ℹ 125 more rows
# # ℹ 16 more variables: url <chr>, args <df[,0]>, headers <df[,4]>, origin <chr>,
# #   id <int>, mpg <dbl>, cyl <int>, disp <dbl>, hp <int>, drat <dbl>, wt <dbl>,
# #   qsec <dbl>, vs <int>, am <int>, gear <int>, carb <int>
# # ℹ Use `print(n = ...)` to see more rows


# query some documents
docdb_query(src, key, query = '{"mpg": {"$gte": 30}}')
#              _id mpg cyl disp  hp drat  wt qsec vs am gear carb
# 1       Fiat 128  32   4   79  66  4.1 2.2   19  1  1    4    1
# 2    Honda Civic  30   4   76  52  4.9 1.6   19  1  1    4    2
# 3 Toyota Corolla  34   4   71  65  4.2 1.8   20  1  1    4    1
# 4   Lotus Europa  30   4   95 113  3.8 1.5   17  1  1    5    2

# query some fields from some documents; 'query' is a mandatory 
# parameter and is used here in its position in the signature
docdb_query(src, key, '{"mpg": {"$gte": 30}}', fields = '{"wt": 1, "mpg": 1}')
#              _id  wt mpg
# 1       Fiat 128 2.2  32
# 2    Honda Civic 1.6  30
# 3   Lotus Europa 1.5  30
# 4 Toyota Corolla 1.8  34

# query some subitem fields from some documents
str(docdb_query(
  src, key, 
  query = '{"$or": [{"age": {"$gt": 21}}, 
           {"friends.name": {"$regex": "^B[a-z]{3,9}.*"}}]}', 
  fields = '{"age": 1, "friends.name": 1}'))
# 'data.frame': 3 obs. of  3 variables:
#  $ _id         : chr  "5cd6785325ce3a94dfc54096" "5cd6785335b63cb19dfa8347" "5cd67853f841025e65ce0ce2"
#  $ age         : int  22 30 23
#  $ friends.name:List of 3
#   ..$ : chr  "Baird Keller" "Francesca Reese" "Dona Bartlett"
#   ..$ : chr  "Coleen Dunn" "Doris Phillips" "Concetta Turner"
#   ..$ : chr  "Wooten Goodwin" "Brandie Woodward" "Angelique Britt"

# such queries can also be used for updating (patching) selected documents 
# with a new 'value'(s) from a JSON string, a data frame or a list
docdb_update(src, key, value = '{"vs": 9, "xy": [1, 2]}', query = '{"carb": 3}')
# [1] 3
docdb_query(src, key, '{"carb": {"$in": [1,3]}}', fields = '{"vs": 1, "_id": 0}')[[1]]
# [1] 1 1 1 9 9 9 1 1 1 1
docdb_get(src, key)[c(3, 109, 130, 101), c("_id", "xy", "url", "email")]
#                                      _id   xy                           url                  email
# 3               5cd6785325ce3a94dfc54096 NULL                          <NA> pacebell@conjurica.com
# 109                     Dodge Challenger NULL                          <NA>                   <NA>
# 130                     Pontiac Firebird NULL                          <NA>                   <NA>
# 101 69bcd195-a59c-11ee-bfb9-acbc328130bb NULL https://httpbin.org/stream/98                   <NA>

# use with dplyr
# *note* that dplyr includes a (deprecated) function src_sqlite
# which would mask nodbi's src_sqlite, so it is excluded here
library("dplyr", exclude = c("src_sqlite", "src_postgres"))
# 
docdb_get(src, key) %>%
  group_by(gear) %>%
  summarise(mean_mpg = mean(mpg))
# # A tibble: 4 × 2
#    gear mean_mpg
#   <int>    <dbl>
# 1     3     16.1
# 2     4     24.5
# 3     5     21.4
# 4    NA     NA 

# delete documents; query is optional parameter and has to be 
# specified for deleting documents instead of deleting the container
dim(docdb_query(src, key, query = '{"$or": [{"age": {"$lte": 20}}, {"age": {"$gte": 25}}]}'))
# [1] 3 11
docdb_delete(src, key, query = '{"$or": [{"age": {"$lte": 20}}, {"age": {"$gte": 25}}]}')
# TRUE
nrow(docdb_get(src, key))
# [1] 132

# delete container from database
docdb_delete(src, key)
# [1] TRUE
# 
# shutdown
DBI::dbDisconnect(src$con, shutdown = TRUE); rm(src)
```

## Benchmark

``` r
library("nodbi")

srcMongo <- src_mongo()
srcSqlite <- src_sqlite()
srcPostgres <- src_postgres()
srcDuckdb <- src_duckdb()
srcElastic <- src_elastic()
srcCouchdb <- src_couchdb(
  user = Sys.getenv("COUCHDB_TEST_USER"), 
  pwd = Sys.getenv("COUCHDB_TEST_PWD"))

key <- "test"
query <- '{"clarity": {"$in": ["SI1", "VS1"]}}'
fields <- '{"cut": 1, "_id": 1, "clarity": "1"}'
value <- '{"clarity": "XYZ", "new": ["ABC", "DEF"]}'
data <- as.data.frame(diamonds)[1:1000, ]
ndjs <- tempfile()
jsonlite::stream_out(iris, con = file(ndjs), verbose = FALSE)

testFunction <- function(src, key, value, query, fields) {
  on.exit(docdb_delete(src, key))
  suppressMessages(docdb_create(src, key, data))
  suppressMessages(docdb_create(src, key, ndjs))
  head(docdb_get(src, key))
  docdb_query(src, key, query = query, fields = fields)
  docdb_update(src, key, value = value, query = query)
}

result <- rbenchmark::benchmark(
  MongoDB = testFunction(src = srcMongo, key, value, query, fields),
  SQLite = testFunction(src = srcSqlite, key, value, query, fields),
  Elastic = testFunction(src = srcElastic, key, value, query, fields),
  CouchDB = testFunction(src = srcCouchdb, key, value, query, fields),
  PostgreSQL = testFunction(src = srcPostgres, key, value, query, fields),
  DuckDB = testFunction(src = srcDuckdb, key, value, query, fields),
  replications = 10L,
  columns = c('test', 'replications', 'elapsed')
)

# 2024-01-19 with 2015 mobile hardware, databases via homebrew
result[rev(order(result$elapsed)), ]
#         test replications elapsed
# 4    CouchDB           10   280.4
# 3    Elastic           10    30.9
# 5 PostgreSQL           10     2.9
# 1    MongoDB           10     1.7
# 6     DuckDB           10     1.6
# 2     SQLite           10     1.5
```

2## Testing {#testing}

Every database backend is subjected to identical tests, see
[core-nodbi.R](https://github.com/ropensci/nodbi/blob/master/tests/testthat/core-nodbi.R).

``` r
#
# 2024-01-19
testthat::test_local()
# ✔ | F W  S  OK | Context
# ✔ |      2 152 | couchdb [99.0s]                                                                   
# ✔ |      1 151 | duckdb [4.2s]                                                                     
# ✔ |      2 150 | elastic [91.5s]                                                                   
# ✔ |      2 150 | mongodb [5.9s]                                                                    
# ✔ |        153 | postgres [11.1s]                                                                  
# ✔ |        154 | sqlite [6.1s]                                                                     
# 
# ══ Results ════════════════════════════════════════════════════════════════════════════════════════
# Duration: 218.1 s
# 
# ── Skipped tests (7) ──────────────────────────────────────────────────────────────────────────────
# • Testing for auto disconnect and shutdown not relevant (3): test-couchdb.R:26:3,
#   test-elastic.R:21:3, test-mongodb.R:24:3
# • Testing for parallel writes not possible or implemented (4): test-couchdb.R:26:3,
#   test-duckdb.R:22:3, test-elastic.R:21:3, test-mongodb.R:24:3
# 
# [ FAIL 0 | WARN 0 | SKIP 7 | PASS 910 ]

# 2024-01-19
covr::package_coverage(type = "all")
# nodbi Coverage: 93.76%
# R/src_duckdb.R: 76.92%
# R/zzz.R: 83.97%
# R/src_mongo.R: 91.30%
# R/update.R: 92.16%
# R/query.R: 94.27%
# R/src_postgres.R: 95.65%
# R/create.R: 96.09%
# R/delete.R: 97.96%
# R/get.R: 98.77%
# R/exists.R: 100.00%
# R/list.R: 100.00%
# R/src_couchdb.R: 100.00%
# R/src_elasticsearch.R: 100.00%
# R/src_sqlite.R: 100.00%
```

## Notes

- Please [report any issues or
  bugs](https://github.com/ropensci/nodbi/issues).
- License: MIT
- Get citation information for `nodbi` in R doing
  `citation(package = 'nodbi')`
- Please note that this package is released with a [Contributor Code of
  Conduct](https://ropensci.org/code-of-conduct/). By contributing to
  this project, you agree to abide by its terms.
- Support for redis has been removed since version 0.5.
