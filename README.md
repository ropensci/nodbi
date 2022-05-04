
# nodbi

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
-   Elasticsearch
-   CouchDB
-   PostgreSQL (since v0.6.0)

for an `R` object of any of these data types:

-   data.frame
-   list
-   JSON string
-   a file name or URL with NDJSON records\*

and for executing the following operations:

-   Create
-   Exists
-   Get
-   Query\*\*
-   Update\*\*
-   Delete
-   List

across all database backends. \* Only for `docdb_create`. \*\*Only
simple queries (e.g. equality for a single field) and updates are
supported for Elasticsearch at the moment. Only root fields can be
specified for CouchDB, whereas subitems can be specified in dot notation
for Elasticsearch, MongoDB, SQLite and PostgreSQL.

For capabilities in terms of operations and parameter combinations
across any of the database backends, see section
[walk-through](#walk-through) below and see the main file for package
testing, here: [core-nodbi.R](./tests/testthat/core-nodbi.R).

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

Parameters for `docdb_*()` functions are the same across database
backends.

| Purpose                                                                                                                         | Function call                                                                                                     |
|---------------------------------------------------------------------------------------------------------------------------------|-------------------------------------------------------------------------------------------------------------------|
| Create database connection (see below)                                                                                          | `src <- nodbi::src_{mongo, sqlite, couchdb, elastic}(<see below for parameters>)`                                 |
| Load `myData` (a data frame, a list, a JSON string, or a file name or url with NDJSON records) into database, container `myTbl` | `nodbi::docdb_create(src = src, key = "myTbl", value = myData)`                                                   |
| Get all documents back into a data frame                                                                                        | `nodbi::docdb_get(src = src, key = "myTbl")`                                                                      |
| Get documents selected with query (as MongoDB-compatible JSON) into a data frame                                                | `nodbi::docdb_query(src = src, key = "myTbl", query = '{"age": 20}')`                                             |
| Get selected fields (in MongoDB compatible JSON) from documents selected query                                                  | `nodbi::docdb_query(src = src, key = "myTbl", query = '{"age": 20}', fields = '{"name": 1, "_id": 0, "age": 1}')` |
| Update (patch) selected documents with new data in data frame, list or JSON string from `myData`                                | `nodbi::docdb_update(src = src, key = "myTbl", value = myData, query = '{"age": 20}')`                            |
| Check if container exists                                                                                                       | `nodbi::docdb_exists(src = src, key = "myTbl")`                                                                   |
| List all containers in database                                                                                                 | `nodbi::docdb_list(src = src)`                                                                                    |
| Delete document(s) in container                                                                                                 | `nodbi::docdb_delete(src = src, key = "myTbl", query = '{"age": 20}')`                                            |
| Delete container                                                                                                                | `nodbi::docdb_delete(src = src, key = "myTbl")`                                                                   |
| Close and remove database connection                                                                                            | `rm(src)`                                                                                                         |

## Database connections

Overview on parameters that are specific to the database backend. These
are only needed once, for `src_*()` to create a connection object that
is used with other `nodbi` functions.

### MongoDB

Note that only MongoDB requires to specify the container already in the
`src_*()` function. “Container” refers to a MongoDB collection.

``` r
nodbi::src_mongo(
 collection = "test", db = "test",
 url = "mongodb://localhost", ...)
```

### SQLite

The functionality to process JSON is based on the SQLite extension
[JSON1](https://www.sqlite.org/json1.html), available in RSQLite.
“Container” refers to an SQLite table.

``` r
nodbi::src_sqlite(dbname = ":memory:", ...)
```

### CouchDB

``` r
nodbi::src_couchdb(
 host = "127.0.0.1", port = 5984L, path = NULL,
 transport = "http", user = NULL, pwd = NULL, headers = NULL)
```

### Elasticsearch

``` r
nodbi::src_elastic(
 host = "127.0.0.1", port = 9200L, path = NULL,
 transport_schema = "http", user = NULL, pwd = NULL, force = FALSE, ...)
```

### PostgreSQL

With this database, the order of variables in data frames returned by
`docdb_get()` and `docdb_query()` can differ from the order in which
they were in `docdb_create()`.

``` r
nodbi::src_postgres(
 dbname = "test", host = "127.0.0.1", port = 5432L, ...)
```

## Walk-through

This example is meant to show how functional `nodbi` is at this time.

``` r
# load nodbi
library(nodbi)

# connect database backend
src <- src_sqlite()

# load data (here data frame, alternatively list or JSON)
# into the container "myTbl" specified in the "key" parameter
docdb_create(src, key = "myTbl", value = mtcars)
#> [1] 32

# load additionally 98 NDJSON records
docdb_create(src, key = "myTbl", "http://httpbin.org/stream/98")
#> Note: container 'myTbl' already exists
#> [1] 98

# load additionally contacts JSON data, from package nodbi
docdb_create(src, key = "myTbl", contacts)
#> Note: container 'myTbl' already exists
#> [1] 5

# get all documents, irrespective of schema
dplyr::tibble(docdb_get(src, "myTbl"))
#> A tibble: 135 × 27
#>    `_id`       isActive balance   age eyeColor name  email about registered tags   friends url  
#>    <chr>       <lgl>    <chr>   <int> <chr>    <chr> <chr> <chr> <chr>      <list> <list>  <chr>
#>  1 5cd678530d… TRUE     $2,412…    20 blue     Kris… kris… Sint… 2017-07-1… <chr>  <df>    NA   
#>  2 5cd678531b… FALSE    $3,400…    20 brown    Rae … raec… Nisi… 2018-12-1… <chr>  <df>    NA   
#>  3 5cd6785325… TRUE     $1,161…    22 brown    Pace… pace… Eius… 2018-08-1… <chr>  <df>    NA   
#>  4 5cd6785335… FALSE    $2,579…    30 brown    Will… will… Null… 2018-02-1… <chr>  <df>    NA   
#>  5 5cd67853f8… FALSE    $3,808…    23 green    Lacy… lacy… Sunt… 2014-08-0… <chr>  <df>    NA   
#>  6 6529d28a-c… NA       NA         NA NA       NA    NA    NA    NA         <NULL> <NULL>  http…
#>  7 6529d2a8-c… NA       NA         NA NA       NA    NA    NA    NA         <NULL> <NULL>  http…
#>  8 6529d2b2-c… NA       NA         NA NA       NA    NA    NA    NA         <NULL> <NULL>  http…
#>  9 6529d2c6-c… NA       NA         NA NA       NA    NA    NA    NA         <NULL> <NULL>  http…
#> 10 6529d2d0-c… NA       NA         NA NA       NA    NA    NA    NA         <NULL> <NULL>  http…
#> # … with 125 more rows, and 15 more variables: args <named list>, headers <df[,4]>,
#> #   origin <chr>, id <int>, mpg <dbl>, cyl <dbl>, disp <dbl>, hp <dbl>, drat <dbl>, wt <dbl>,
#> #   qsec <dbl>, vs <dbl>, am <dbl>, gear <dbl>, carb <dbl>

# query some documents
docdb_query(src, "myTbl", query = '{"mpg": {"$gte": 30}}')
#>              _id mpg cyl disp  hp drat  wt qsec vs am gear carb
#> 1       Fiat 128  32   4   79  66  4.1 2.2   19  1  1    4    1
#> 2    Honda Civic  30   4   76  52  4.9 1.6   19  1  1    4    2
#> 3   Lotus Europa  30   4   95 113  3.8 1.5   17  1  1    5    2
#> 4 Toyota Corolla  34   4   71  65  4.2 1.8   20  1  1    4    1

# query some fields from some documents; 'query' is a mandatory 
# parameter and is used here in its position in the signature
docdb_query(src, "myTbl", '{"mpg": {"$gte": 30}}', fields = '{"wt": 1, "mpg": 1}')
#>    wt mpg
#> 1 2.2  32
#> 2 1.6  30
#> 3 1.5  30
#> 4 1.8  34

# query some subitem fields from some documents
# (only simple queries so far implemented for Elasticsearch)
# (only root, not subitems so far implemented for CouchDB)
str(docdb_query(src, "myTbl", '
 {"$or": [{"age": {"$lt": 21}}, 
 {"friends.name": {"$regex": "^B[a-z]{3,6}.*"}}]}', 
 fields = '{"age": 1, "friends.name": 1}'))
#> 'data.frame':    4 obs. of 2 variables:
#> $ age : int 20 20 22 23
#> $ friends:'data.frame':  4 obs. of 1 variable:
#> ..$ name:List of 4
#> .. ..$ : chr "Pace Bell"
#> .. ..$ : chr "Yang Yates" "Lacy Chen"
#> .. ..$ : chr "Baird Keller" "Francesca Reese" "Dona Bartlett"
#> .. ..$ : chr "Wooten Goodwin" "Brandie Woodward" "Angelique Britt"

# such queries can also be used for updating (patching) selected documents 
# with a new 'value'(s) from a JSON string, a data frame or a list
docdb_update(src, "myTbl", value = '{"vs": 9, "xy": [1, 2]}', query = '{"carb": 3}')
#> [1] 3
docdb_query(src, "myTbl", '{"carb": {"$in": [1,3]}}', fields = '{"vs": 1}')[[1]]
#> [1] 1 1 1 1 9 9 9 1 1 1
docdb_get(src, "myTbl")[126:130, c(1, 27, 28)]
#>                  _id carb   xy
#> 126        Merc 280C    4 NULL
#> 127       Merc 450SE    3 1, 2
#> 128       Merc 450SL    3 1, 2
#> 129      Merc 450SLC    3 1, 2
#> 130 Pontiac Firebird    2 NULL

# use with dplyr
library("dplyr")
docdb_get(src, "myTbl") %>%
 group_by(gear) %>%
 summarise(mean_mpg = mean(mpg))
#> # A tibble: 4 × 2
#>    gear mean_mpg
#>   <dbl>    <dbl>
#> 1     3     16.1
#> 2     4     24.5
#> 3     5     21.4
#> 4    NA     NA

# delete documents; query is optional parameter and has to be 
# specified for deleting documents instead of deleting the container
docdb_delete(src, "myTbl", query = '{"$or": {"gear": 5, "age": {"$gte": 22}}}')
#> TRUE
nrow(docdb_get(src, "myTbl"))
#> [1] 127

# delete container from database
docdb_delete(src, "myTbl")
#> [1] TRUE
```

## Benchmark

``` r
library("nodbi")

COUCHDB_TEST_USER <- Sys.getenv("COUCHDB_TEST_USER")
COUCHDB_TEST_PWD <- Sys.getenv("COUCHDB_TEST_PWD")

srcMongo <- src_mongo()
srcSqlite <- src_sqlite() # here, in-memory database
srcElastic <- src_elastic()
srcCouchdb <- src_couchdb(user = COUCHDB_TEST_USER, pwd = COUCHDB_TEST_PWD)
srcPostgres <- src_postgres()
key <- "test"
query <- '{"clarity": "SI1"}'
fields <- '{"cut": 1, "_id": 1, "clarity": "1"}'
value <- '{"clarity": "XYZ", "new": ["ABC", "DEF"]}'
data <- as.data.frame(diamonds)[1:12000, ]

testFunction <- function(src, key, value, query, fields) {
 docdb_create(src, key, data)
 # Elasticsearch needs a delay to process the data
 docdb_create(src, key, "http://httpbin.org/stream/89")
 # Elasticsearch needs a delay to process the data
 if (inherits(src, "src_elastic")) Sys.sleep(1)
 head(docdb_get(src, key))
 docdb_query(src, key, query = query, fields = fields)
 docdb_update(src, key, value = value, query = query)
 docdb_delete(src, key)
}

#> 2022-05-04 with 2015 mobile hardware 
#> without any database optimisations
rbenchmark::benchmark(
 MongoDB = testFunction(src = srcMongo, key, value, query, fields),
 RSQLite = testFunction(src = srcSqlite, key, value, query, fields),
 Elastic = testFunction(src = srcElastic, key, value, query, fields),
 CouchDB = testFunction(src = srcCouchdb, key, value, query, fields),
 PostgreSQL = testFunction(src = srcPostgres, key, value, query, fields),
 replications = 10L,
 columns = c('test', 'replications', 'elapsed')
)
#>         test replications elapsed
#> 4    CouchDB           10    1639
#> 3    Elastic           10     183 # 10s to be subtracted
#> 1    MongoDB           10      74
#> 5 PostgreSQL           10      81
#> 2    RSQLite           10      76
```

## Testing

``` r
testthat::test_local()
```

## Notes

-   Please [report any issues or
    bugs](https://github.com/ropensci/nodbi/issues).
-   License: MIT
-   Get citation information for `nodbi` in R doing
    `citation(package = 'nodbi')`
-   Please note that this package is released with a [Contributor Code
    of Conduct](https://ropensci.org/code-of-conduct/). By contributing
    to this project, you agree to abide by its terms.
-   Support for redis has been removed since version 0.5, because no way
    was found to query and update specific documents in a container.
