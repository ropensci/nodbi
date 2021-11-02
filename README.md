
# nodbi

<!--

-->

[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![cran
checks](https://cranchecks.info/badges/worst/nodbi)](https://cranchecks.info/pkgs/nodbi)
[![R-CMD-check](https://github.com/ropensci/nodbi/workflows/R-CMD-check/badge.svg)](https://github.com/ropensci/nodbi/actions?query=workflow%3AR-CMD-check)
[![codecov](https://codecov.io/gh/ropensci/nodbi/branch/master/graph/badge.svg)](https://codecov.io/gh/ropensci/nodbi)
[![rstudio mirror
downloads](http://cranlogs.r-pkg.org/badges/nodbi)](https://github.com/r-hub/cranlogs.app)
[![cran
version](https://www.r-pkg.org/badges/version/nodbi)](https://cran.r-project.org/package=nodbi)

`nodbi` is an R package that provides a single interface for several
NoSQL databases, with the same function parameters and return values
across backends.

Currently, `nodbi` supports the following database backends:

-   MongoDB
-   SQLite
-   CouchDB
-   Elasticsearch

for an `R` object of any of these data types:

-   data.frame
-   list
-   JSON string

and for executing the following operations:

-   Create
-   Exists
-   Get
-   Query\* \*\*
-   Update\* \*\*
-   Delete
-   List

across all database backends. \*Only simple queries (and updates, e.g.
equality for a single field) supported for Elasticsearch at the moment.
\*\*Only root fields can be specified for CouchDB and Elasticsearch,
whereas subitems in fields can be specified for MongoDB and SQLite.

For details of operations and parameter combinations across any of the
database backends, see the main file for package testing, here:
[core-nodbi.R](./tests/testthat/core-nodbi.R).

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

## API overview

Parameters for `docdb_*()` functions are the same across functions.

| Purpose                                                                                          | Function call                                                                                                     |
|--------------------------------------------------------------------------------------------------|-------------------------------------------------------------------------------------------------------------------|
| Create database connection (see below)                                                           | `src <- nodbi::src_{mongo, sqlite, couchdb, elastic}(<see below for parameters>)`                                 |
| Load data frame, list or JSON string from `myData` into database, table `dbTbl`                  | `nodbi::docdb_create(src = src, key = "dbTbl", value = myData)`                                                   |
| Get all documents back into a data frame                                                         | `nodbi::docdb_get(src = src, key = "dbTbl")`                                                                      |
| Get documents selected with query (as MongoDB-compatible JSON) into a data frame                 | `nodbi::docdb_query(src = src, key = "dbTbl", query = '{"age": 20}')`                                             |
| Get selected fields (in MongoDB compatible JSON) from documents selected query                   | `nodbi::docdb_query(src = src, key = "dbTbl", query = '{"age": 20}', fields = '{"name": 1, "_id": 0, "age": 1}')` |
| Update (patch) selected documents with new data in data frame, list or JSON string from `myData` | `nodbi::docdb_update(src = src, key = "dbTbl", value = myData, query = '{"age": 20}')`                            |
| Check if table exists                                                                            | `nodbi::docdb_exists(src = src, key = "dbTbl")`                                                                   |
| List all tables in database                                                                      | `nodbi::docdb_list(src = src)`                                                                                    |
| Delete document(s) in table                                                                      | `nodbi::docdb_delete(src = src, key = "dbTbl", query = '{"age": 20}')`                                            |
| Delete database                                                                                  | `nodbi::docdb_delete(src = src, key = "dbTbl")`                                                                   |
| Close and remove database connection                                                             | `rm(src)`                                                                                                         |

## Database connections

Overview on parameters that are specific to the database backend. These
are only needed once, for `src_*()` to create a connection object for
use with `nodbi`.

### MongoDB

(Note that only MongoDB requires to specify the table already in the
`src_*()` function.)

``` r
src_mongo(
  collection = "test", db = "test",
  url = "mongodb://localhost", ...)
```

### SQLite

The functionality to process JSON is based on the SQLite extension
[JSON1](https://www.sqlite.org/json1.html), available in RSQLite.

``` r
src_sqlite(dbname = ":memory:", ...)
```

### CouchDB

``` r
src_couchdb(
  host = "127.0.0.1", port = 5984, path = NULL,
  transport = "http", user = NULL, pwd = NULL, headers = NULL)
```

### Elasticsearch

``` r
src_elastic(
  host = "127.0.0.1", port = 9200, path = NULL,
  transport_schema = "http", user = NULL, pwd = NULL, force = FALSE, ...)
```

## Walk-through

This example is meant to show how functional `nodbi` is at this time.

``` r
# connect database backend
src <- src_sqlite()

# load data (here data frame, alternatively list or JSON)
docdb_create(src, key = "myTbl", value = mtcars)
#> [1] 32

# load additionally contacts JSON data, from package nodbi
docdb_create(src, key = "myTbl", contacts)
#> Note: container 'myTbl' already exists
#> [1] 5

# get all documents, irrespective of schema
dplyr::tibble(docdb_get(src, "myTbl"))
#> # A tibble: 37 × 22
#>    `_id`  isActive balance    age eyeColor name  email about  registered
#>    <chr>  <lgl>    <chr>    <int> <chr>    <chr> <chr> <chr>  <chr>     
#>  1 5cd67… TRUE     $2,412.…    20 blue     Kris… kris… Sint … 2017-07-1…
#>  2 5cd67… FALSE    $3,400.…    20 brown    Rae … raec… Nisi … 2018-12-1…
# ...
#>  9 Chrys… NA       NA          NA NA       NA    NA    NA     NA        
#> 10 Datsu… NA       NA          NA NA       NA    NA    NA     NA        
#> # … with 27 more rows, and 13 more variables: tags <list>,
#> #   friends <list>, mpg <dbl>, cyl <dbl>, disp <dbl>, hp <dbl>,
#> #   drat <dbl>, wt <dbl>, qsec <dbl>, vs <dbl>, am <dbl>, gear <dbl>,
#> #   carb <dbl>

# query some documents
docdb_query(src, "myTbl", query = '{"mpg": {"$gte": 30}}')
#>              _id mpg cyl disp  hp drat  wt qsec vs am gear carb
#> 1       Fiat 128  32   4   79  66  4.1 2.2   19  1  1    4    1
#> 2    Honda Civic  30   4   76  52  4.9 1.6   19  1  1    4    2
#> 3   Lotus Europa  30   4   95 113  3.8 1.5   17  9  1    5    2
#> 4 Toyota Corolla  34   4   71  65  4.2 1.8   20  1  1    4    1

# query some fields from some documents; 
# query is mandatory parameter and is used
# her ein its position in the signature
docdb_query(src, "myTbl", '{"mpg": {"$gte": 30}}', fields = '{"wt": 1, "mpg": 1}')
#>    wt mpg
#> 1 2.2  32
#> 2 1.6  30
#> 3 1.5  30
#> 4 1.8  34

# query some subitem fields from some documents
str(docdb_query(src, "myTbl", '{"age": {"$gte": 22}}', 
                fields = '{"age": 1, "friends.name": 1}'))
#> 'data.frame':    3 obs. of  2 variables:
#>  $ age    : int  22 30 23
#>  $ friends:'data.frame': 3 obs. of  1 variable:
#>   ..$ name:List of 3
#>   .. ..$ : chr  "Baird Keller" "Francesca Reese" "Dona Bartlett"
#>   .. ..$ : chr  "Coleen Dunn" "Doris Phillips" "Concetta Turner"
#>   .. ..$ : chr  "Wooten Goodwin" "Brandie Woodward" "Angelique Britt"

# update some data; needs four parameters
docdb_update(src, "myTbl", value = '{"vs": 9}', query = '{"carb": 3}')
#> [1] 3
docdb_query(src, "myTbl", '{"carb": {"$in": [1,3]}}', fields = '{"vs": 1}')[[1]]
#> [1] 1 1 1 1 9 9 9 1 1 1

# use with dplyr
library("dplyr")
docdb_get(src, "myTbl") %>%
  group_by(gear) %>%
  summarise(mean_mpg = mean(mpg))
# # A tibble: 4 × 2
#    gear mean_mpg
#   <dbl>    <dbl>
# 1     3     16.1
# 2     4     24.5
# 3     5     21.4
# 4    NA     NA  

# delete documents; query is optional parameter and thus has to be specified 
# to delete documents; the complex query supported only by MongoDB and RSQLite
docdb_delete(src, "myTbl", query = '{"$or": {"gear": 5, "age": {"$gte": 22}}}')
#> TRUE
nrow(docdb_get(src, "myTbl"))
#> [1] 29

# delete database
docdb_delete(src, "myTbl")
#> TRUE
```

## Benchmark

``` r
library("nodbi")

COUCHDB_TEST_USER <- Sys.getenv("COUCHDB_TEST_USER")
COUCHDB_TEST_PWD <- Sys.getenv("COUCHDB_TEST_PWD")

srcMongo <- src_mongo()
srcSqlite <- src_sqlite()
srcElastic <- src_elastic()
srcCouchdb <- src_couchdb(user = COUCHDB_TEST_USER, pwd = COUCHDB_TEST_PWD)
key <- "test"
query <- '{"clarity": "I1"}'
fields <- '{"cut": 1, "_id": 1}'
value <- '{"clarity": "XYZ"}'
data <- as.data.frame(diamonds)[1:2000,]

testFunction <- function(src, key, value, query, fields) {
  docdb_create(src, key, data)
  # Elastic needs a moment to process the data
  if (inherits(src, "src_elastic")) Sys.sleep(1)
  head(docdb_get(src, key))
  docdb_query(src, key, query = query, fields = fields)
  docdb_update(src, key, value = value, query = query)
  docdb_delete(src, key)
}

rbenchmark::benchmark(
  MongoDB = testFunction(src = srcMongo, key, value, query, fields),
  RSQLite = testFunction(src = srcSqlite, key, value, query, fields),
  Elastic = testFunction(src = srcElastic, key, value, query, fields),
  CouchDB = testFunction(src = srcCouchdb, key, value, query, fields),
  replications = 10L,
  columns = c('test', 'replications', 'elapsed')
)
#> results with 10 s subtracted for Elastic
#>      test replications elapsed
#> 4 CouchDB           10    48.2
#> 3 Elastic           10    28.2
#> 1 MongoDB           10     4.5
#> 2 RSQLite           10     4.0
```

## Testing

``` r
testthat::test_local()
```

    ✓ | F W S  OK | Context
    ✓ |         5 | couchdb [0.6s]                                                                            
    ✓ |        27 | - create, exists, list, get, delete [2.2s]                                                
    ✓ |        26 | - query [1.1s]                                                                            
    ✓ |        14 | - update, query [5.1s]                                                                    
    ✓ |         3 | elastic [0.8s]                                                                            
    ✓ |        27 | - create, exists, list, get, delete [16.4s]                                               
    ✓ |     1  10 | - query [2.2s]                                                                            
    ──────────────────────────────────────────────────────────────
    Skip (core-nodbi.R:106:3): docdb_query
    Reason: queries need to be translated into elastic syntax
    ──────────────────────────────────────────────────────────────
    ✓ |        12 | - update, query [3.6s]                                                                    
    ✓ |         3 | mongodb [0.3s]                                                                            
    ✓ |        27 | - create, exists, list, get, delete [0.8s]                                                
    ✓ |        26 | - query [0.3s]                                                                            
    ✓ |        14 | - update, query [0.1s]                                                                    
    ✓ |         3 | sqlite                                                                                    
    ✓ |        27 | - create, exists, list, get, delete [0.5s]                                                
    ✓ |        26 | - query [0.2s]                                                                            
    ✓ |        14 | - update, query [0.1s]                                                                    

    ══ Results ══════════════════════════════════════════
    Duration: 34.3 s

    ── Skipped tests  ───────────────────────────────────────────
    • queries need to be translated into elastic syntax (1)

    [ FAIL 0 | WARN 0 | SKIP 1 | PASS 264 ]

## Notes

-   Please [report any issues or
    bugs](https://github.com/ropensci/nodbi/issues).
-   License: MIT
-   Get citation information for `nodbi` in R doing
    `citation(package = 'nodbi')`
-   Please note that this package is released with a [Contributor Code
    of Conduct](https://ropensci.org/code-of-conduct/). By contributing
    to this project, you agree to abide by its terms.
-   Support for redis has been removed for version 0.5, because no way
    was found to query and update specific documents in a container.
