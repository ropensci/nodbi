# nodbi

⁠
[![R-CMD-check](https://github.com/ropensci/nodbi/workflows/R-CMD-check/badge.svg)](https://github.com/ropensci/nodbi/actions?query=workflow%3AR-CMD-check)
[![codecov](https://codecov.io/gh/rfhb/nodbi/branch/master/graph/badge.svg)](https://app.codecov.io/gh/rfhb/nodbi)
[![CRAN
status](https://www.r-pkg.org/badges/version/nodbi)](https://CRAN.R-project.org/package=nodbi)
[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
⁠

`nodbi` is an R package that provides a single interface for several
NoSQL databases and databases with JSON functionality, with the same
function parameters and return values across all database backends. Last
updated 2026-08-11.

[TABLE]

For speed comparisons of database backends, see [benchmark](#benchmark)
and [testing](#testing) below.

## API overview

Parameters for `docdb_*()` functions are the same across all database
backends. See [walk-through](#walk-through) below and the canonical
testing in
[core-nodbi.R](https://docs.ropensci.org/nodbi/tests/testthat/core-nodbi.R).
“Container” is used as term to indicate where conceptually the backend
holds the data, see [Database connections](#database-connections) below.
The `key` parameter holds the name of a container.

| Purpose | Function call |
|:---|:---|
| Create database connection (see below) | `src <- nodbi::src_{duckdb, postgres, mongo, sqlite, couchdb, elastic}(<see below for parameters>)` |
| Load `my_data` (a data frame, list, JSON string, or file name or URL pointing to NDJSON records) into database, container `my_container` | `nodbi::docdb_create(src = src, key = "my_container", value = my_data)` |
| Get all documents back into a data frame | `nodbi::docdb_get(src = src, key = "my_container")` |
| Get documents selected with query (as MongoDB-compatible JSON) into a data frame | `nodbi::docdb_query(src = src, key = "my_container", query = '{"age": 20}')` |
| Get selected fields (in MongoDB compatible JSON) from documents selected by query into a data frame | `nodbi::docdb_query(src = src, key = "my_container", query = '{"age": {"$gt": 20}}', fields = '{"friends.name": 1, "_id": 0, "age": 1}', limit = 2L)` |
| Update (patch) documents selected by query with new data `my_data` (in a data frame, list, JSON string, or file name or URL pointing to NDJSON records) | `nodbi::docdb_update(src = src, key = "my_container", value = my_data, query = '{"age": 20}')` |
| Check if container exists | `nodbi::docdb_exists(src = src, key = "my_container")` |
| List all containers in database | `nodbi::docdb_list(src = src)` |
| Delete document(s) in container | `nodbi::docdb_delete(src = src, key = "my_container", query = '{"age": 20}')` |
| Delete container | `nodbi::docdb_delete(src = src, key = "my_container")` |
| Close and remove database connection manually (when restarting R, connections are automatically closed and removed by `nodbi`) | `rm(src)` |

## Install

CRAN version

[`install.packages`](https://rdrr.io/r/utils/install.packages.html)`(``"nodbi"``)`

Development version

`remotes``::`[`install_github`](https://remotes.r-lib.org/reference/install_github.html)`(``"ropensci/nodbi"``)`

Load package from library

[`library`](https://rdrr.io/r/base/library.html)`(`[`"nodbi"`](https://docs.ropensci.org/nodbi/)`)`

## Database connections

The following subsections show those parameters and aspects that are
specific to the database backend. These are only needed once, for
`src_*()` to create a connection object. Any such connection object is
subsequently used similarly across the `docdb_*` functions in package
‘nodbi’.

“Container” refers to how conceptually the database backend holds the
data. Users specify the relevant container with parameter
`key = <container_name>` in `docdb_*` functions, see
[Walk-through](#walk-through) below).

Data types are mapped from JSON to R objects by
[jsonlite](https://CRAN.R-project.org/package=jsonlite). Any root-level
`_id` is extracted from the document(s) and used for an index column
`_id`, otherwise an UUID is created as `_id`.

### DuckDB

See also <https://CRAN.R-project.org/package=duckdb>. “Container” refers
to a DuckDB table, with columns `_id` and `json` created and used by
package `nodbi`, applying SQL functions and functions as per
<https://duckdb.org/docs/extensions/json> to the `json` column. Each row
in the table represents a `JSON` document.

`src`` ``<-`` ``nodbi``::`[`src_duckdb`](https://docs.ropensci.org/nodbi/reference/src_duckdb.md)`(``dbdir ``=`` ``":memory:"``, ``...``)`

### MongoDB

“Container” refers to a MongoDB collection, in which `nodbi` creates
JSON documents. See also <https://jeroen.github.io/mongolite/>. MongoDB
but none of the other databases require to specify the container name
already in the `src_*()` function; use the `collection` name for
parameter `key` in `docdb_*` functions.

`src`` ``<-`` ``nodbi``::`[`src_mongo`](https://docs.ropensci.org/nodbi/reference/src_mongo.md)`(`` `` collection ``=`` ``"my_container"``, db ``=`` ``"my_database"``,`` `` url ``=`` ``"mongodb://localhost"``, ``...`` ``)`

### SQLite

“Container” refers to an SQLite table, with columns `_id` and `json`
created and used by package `nodbi`, applying SQL functions and
functions as per <https://www.sqlite.org/json1.html> to the `json`
column. Each row in the table represents a `JSON` document. The table is
indexed on `_id`. See also <https://CRAN.R-project.org/package=RSQLite>.

`src`` ``<-`` ``nodbi``::`[`src_sqlite`](https://docs.ropensci.org/nodbi/reference/src_sqlite.md)`(``dbname ``=`` ``":memory:"``, ``...``)`

### PostgreSQL

“Container” refers to a PostgreSQL table, with columns `_id` and `json`
created and used by package `nodbi`, applying SQL and JSON functions as
per <https://www.postgresql.org/docs/current/functions-json.html> to the
`json` column. With PostgreSQL, a custom `plpgsql` function
[jsonb_merge_patch()](https://github.com/ropensci/nodbi/blob/master/R/src_postgres.R#L75)
is used for
[`docdb_update()`](https://docs.ropensci.org/nodbi/reference/docdb_update.md).
The order of variables in data frames returned by
[`docdb_get()`](https://docs.ropensci.org/nodbi/reference/docdb_get.md)
and
[`docdb_query()`](https://docs.ropensci.org/nodbi/reference/docdb_query.md)
can differ from their order the input to
[`docdb_create()`](https://docs.ropensci.org/nodbi/reference/docdb_create.md).

`src`` ``<-`` ``nodbi``::`[`src_postgres`](https://docs.ropensci.org/nodbi/reference/src_postgres.md)`(`` `` dbname ``=`` ``"my_database"``, host ``=`` ``"127.0.0.1"``, port ``=`` ``5432L``, ``...`` ``)`

### CouchDB

“Container” refers to a CouchDB database, in which `nodbi` creates JSON
documents. See also <https://CRAN.R-project.org/package=sofa>. With
CouchDB, function
[`docdb_update()`](https://docs.ropensci.org/nodbi/reference/docdb_update.md)
uses [jqr](https://cran.r-project.org/package=jqr) to implement patching
JSON, in analogy to functions available for the other databases.

`src`` ``<-`` ``nodbi``::`[`src_couchdb`](https://docs.ropensci.org/nodbi/reference/src_couchdb.md)`(`` `` host ``=`` ``"127.0.0.1"``, port ``=`` ``5984L``, path ``=`` ``NULL``,`` `` transport ``=`` ``"http"``, user ``=`` ``NULL``, pwd ``=`` ``NULL``, headers ``=`` ``NULL`` ``)`

### Elasticsearch

“Container” refers to an Elasticsearch index, in which `nodbi` creates
JSON documents. Opensearch can equally be used. Only lowercase is
accepted for container names (in parameter `key` of `docdb_*`
functions).

`src`` ``<-`` ``nodbi``::`[`src_elastic`](https://docs.ropensci.org/nodbi/reference/src_elastic.md)`(`` `` host ``=`` ``"127.0.0.1"``, port ``=`` ``9200L``, path ``=`` ``NULL``,`` `` transport_schema ``=`` ``"http"``, user ``=`` ``NULL``, pwd ``=`` ``NULL``, ``...`` ``)`

### MariaDB

“Container” refers to a MariaDB table, with columns `_id` and `json`
created and used by package `nodbi`, applying SQL and JSON functions as
per
<https://mariadb.com/docs/server/reference/sql-functions/special-functions/json-functions>
to the `json` column. Functions
[`docdb_create()`](https://docs.ropensci.org/nodbi/reference/docdb_create.md)
and
[`docdb_update()`](https://docs.ropensci.org/nodbi/reference/docdb_update.md)
can import from an NDJSON file name if the MariaDB server is configured
accordingly (typically if run on the same system); otherwise they fall
back to a slower import method.

`src`` ``<-`` ``nodbi``::`[`src_mariadb`](https://docs.ropensci.org/nodbi/reference/src_mariadb.md)`(`` `` dbname ``=`` ``"my_database"``, ``...`` ``)`` ``# by default, connects to MariaDB server on localhost`` ``` # `...` any named parameters passed to RMariaDB::MariaDB(), see ``` ``# help("MariaDB", "RMariaDB")`

## Walk-through

This example is to show how functional `nodbi` is at this time: With any
of the six database backends, the functions work in the same way and
return the same values.

`# load nodbi`` `[`library`](https://rdrr.io/r/base/library.html)`(`[`nodbi`](https://docs.ropensci.org/nodbi/)`)`` `` ``# name of container`` ``key`` ``<-`` ``"my_container"`` `` ``# connect any of these database backends`` `[`options`](https://rdrr.io/r/base/options.html)`(``duckdb.extension_directory ``=`` ``"~/.duckdb_extensions"``)`` ``src`` ``<-`` `[`src_duckdb`](https://docs.ropensci.org/nodbi/reference/src_duckdb.md)`(``)`` ``src`` ``<-`` `[`src_mongo`](https://docs.ropensci.org/nodbi/reference/src_mongo.md)`(``collection ``=`` ``key``)`` ``src`` ``<-`` `[`src_sqlite`](https://docs.ropensci.org/nodbi/reference/src_sqlite.md)`(``)`` ``src`` ``<-`` `[`src_postgres`](https://docs.ropensci.org/nodbi/reference/src_postgres.md)`(``)`` ``src`` ``<-`` `[`src_mariadb`](https://docs.ropensci.org/nodbi/reference/src_mariadb.md)`(``)`` ``src`` ``<-`` `[`src_elastic`](https://docs.ropensci.org/nodbi/reference/src_elastic.md)`(``)`` ``src`` ``<-`` `[`src_couchdb`](https://docs.ropensci.org/nodbi/reference/src_couchdb.md)`(`` `` user ``=`` `[`Sys.getenv`](https://rdrr.io/r/base/Sys.getenv.html)`(``"COUCHDB_TEST_USER"``)``,`` `` pwd ``=`` `[`Sys.getenv`](https://rdrr.io/r/base/Sys.getenv.html)`(``"COUCHDB_TEST_PWD"``)`` ``)`` ``src`` ``<-`` `[`src_mariadb`](https://docs.ropensci.org/nodbi/reference/src_mariadb.md)`(``)`` `` ``# check if container already exists`` `[`docdb_exists`](https://docs.ropensci.org/nodbi/reference/docdb_exists.md)`(``src``, ``key``)`` ``# [1] FALSE`` `` ``# load data (here data frame, alternatively a list, JSON or file with NSJSON)`` ``# into the container "my_container" specified in "key" parameter`` `[`docdb_create`](https://docs.ropensci.org/nodbi/reference/docdb_create.md)`(``src``, ``key``, value ``=`` ``mtcars``)`` ``# [1] 32`` `` ``# load additionally 98 NDJSON records`` `[`docdb_create`](https://docs.ropensci.org/nodbi/reference/docdb_create.md)`(``src``, ``key``, ``"https://httpbin.org/stream/98"``)`` ``# Note: container 'my_container' already exists`` ``# [1] 98`` `` ``# load additionally contacts JSON data, from package nodbi`` `[`docdb_create`](https://docs.ropensci.org/nodbi/reference/docdb_create.md)`(``src``, ``key``, ``contacts``)`` ``# Note: container 'my_container' already exists`` ``# [1] 5`` `` ``# get all documents, irrespective of schema`` ``dplyr``::`[`tibble`](https://tibble.tidyverse.org/reference/tibble.html)`(`[`docdb_get`](https://docs.ropensci.org/nodbi/reference/docdb_get.md)`(``src``, ``key``)``)`` ``# # A tibble: 135 × 27`` ``` # `_id` isActive balance age eyeColor name email about registered tags friends ``` ``# <chr> <lgl> <chr> <int> <chr> <chr> <chr> <chr> <chr> <list> <list>`` ``# 1 5cd6… TRUE $2,412… 20 blue Kris… kris… "Sin… 2017-07-1… <chr> <df>`` ``# 2 5cd6… FALSE $3,400… 20 brown Rae … raec… "Nis… 2018-12-1… <chr> <df>`` ``# 3 5cd6… TRUE $1,161… 22 brown Pace… pace… "Eiu… 2018-08-1… <chr> <df>`` ``# 4 5cd6… FALSE $2,579… 30 brown Will… will… "Nul… 2018-02-1… <chr> <df>`` ``# 5 5cd6… FALSE $3,808… 23 green Lacy… lacy… "Sun… 2014-08-0… <chr> <df>`` ``# 6 69bc… NA NA NA NA NA NA NA NA <NULL> <NULL>`` ``# 7 69bc… NA NA NA NA NA NA NA NA <NULL> <NULL>`` ``# 8 69bc… NA NA NA NA NA NA NA NA <NULL> <NULL>`` ``# 9 69bc… NA NA NA NA NA NA NA NA <NULL> <NULL>`` ``# 10 69bc… NA NA NA NA NA NA NA NA <NULL> <NULL>`` ``# # ℹ 125 more rows`` ``# # ℹ 16 more variables: url <chr>, args <df[,0]>, headers <df[,4]>, origin <chr>,`` ``# # id <int>, mpg <dbl>, cyl <int>, disp <dbl>, hp <int>, drat <dbl>, wt <dbl>,`` ``# # qsec <dbl>, vs <int>, am <int>, gear <int>, carb <int>`` ``` # # ℹ Use `print(n = ...)` to see more rows ``` `` `` ``# query some documents`` `[`docdb_query`](https://docs.ropensci.org/nodbi/reference/docdb_query.md)`(``src``, ``key``, query ``=`` ``'{"mpg": {"$gte": 30}}'``)`` ``# _id mpg cyl disp hp drat wt qsec vs am gear carb`` ``# 1 Fiat 128 32 4 79 66 4.1 2.2 19 1 1 4 1`` ``# 2 Honda Civic 30 4 76 52 4.9 1.6 19 1 1 4 2`` ``# 3 Toyota Corolla 34 4 71 65 4.2 1.8 20 1 1 4 1`` ``# 4 Lotus Europa 30 4 95 113 3.8 1.5 17 1 1 5 2`` `` ``# query some fields from some documents; 'query' is a mandatory`` ``# parameter and is used here in its position in the signature`` `[`docdb_query`](https://docs.ropensci.org/nodbi/reference/docdb_query.md)`(``src``, ``key``, ``'{"mpg": {"$gte": 30}}'``, fields ``=`` ``'{"wt": 1, "mpg": 1}'``)`` ``# _id wt mpg`` ``# 1 Fiat 128 2.2 32`` ``# 2 Honda Civic 1.6 30`` ``# 3 Lotus Europa 1.5 30`` ``# 4 Toyota Corolla 1.8 34`` `` ``# query some subitem fields from some documents`` `[`str`](https://rdrr.io/r/utils/str.html)`(`[`docdb_query`](https://docs.ropensci.org/nodbi/reference/docdb_query.md)`(`` `` ``src``, ``key``,`` `` query ``=`` ``'{"$or": [{"age": {"$gt": 21}},`` `` {"friends.name": {"$regex": "^B[a-z]{3,9}.*"}}]}'``,`` `` fields ``=`` ``'{"age": 1, "friends.name": 1}'`` ``)``)`` ``# 'data.frame': 3 obs. of 3 variables:`` ``# $ _id : chr "5cd6785325ce3a94dfc54096" "5cd6785335b63cb19dfa8347" "5cd67853f841025e65ce0ce2"`` ``# $ age : int 22 30 23`` ``# $ friends.name:List of 3`` ``# ..$ : chr "Baird Keller" "Francesca Reese" "Dona Bartlett"`` ``# ..$ : chr "Coleen Dunn" "Doris Phillips" "Concetta Turner"`` ``# ..$ : chr "Wooten Goodwin" "Brandie Woodward" "Angelique Britt"`` `` ``# such queries can also be used for updating (patching) selected documents`` ``# with a new 'value'(s) from a JSON string, a data frame a list or a file with NSJSON)`` `[`docdb_update`](https://docs.ropensci.org/nodbi/reference/docdb_update.md)`(``src``, ``key``, value ``=`` ``'{"vs": 9, "xy": [1, 2]}'``, query ``=`` ``'{"carb": 3}'``)`` ``# [1] 3`` `[`docdb_query`](https://docs.ropensci.org/nodbi/reference/docdb_query.md)`(``src``, ``key``, ``'{"carb": {"$in": [1,3]}}'``, fields ``=`` ``'{"vs": 1, "_id": 0}'``)``[[``1``]``]`` ``# [1] 1 1 1 9 9 9 1 1 1 1`` `[`docdb_get`](https://docs.ropensci.org/nodbi/reference/docdb_get.md)`(``src``, ``key``)``[`[`c`](https://rdrr.io/r/base/c.html)`(``3``, ``109``, ``130``, ``101``)``, `[`c`](https://rdrr.io/r/base/c.html)`(``"_id"``, ``"xy"``, ``"url"``, ``"email"``)``]`` ``# _id xy url email`` ``# 3 5cd6785325ce3a94dfc54096 NULL <NA> pacebell@conjurica.com`` ``# 109 Dodge Challenger NULL <NA> <NA>`` ``# 130 Pontiac Firebird NULL <NA> <NA>`` ``# 101 69bcd195-a59c-11ee-bfb9-acbc328130bb NULL https://httpbin.org/stream/98 <NA>`` `` ``# use with dplyr`` ``# *note* that dplyr includes a (deprecated) function src_sqlite`` ``# which would mask nodbi's src_sqlite, so it is excluded here`` `[`library`](https://rdrr.io/r/base/library.html)`(`[`"dplyr"`](https://dplyr.tidyverse.org)`, exclude ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"src_sqlite"``, ``"src_postgres"``)``)`` ``#`` `[`docdb_get`](https://docs.ropensci.org/nodbi/reference/docdb_get.md)`(``src``, ``key``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `` `[`group_by`](https://dplyr.tidyverse.org/reference/group_by.html)`(``gear``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `` `[`summarise`](https://dplyr.tidyverse.org/reference/summarise.html)`(``mean_mpg ``=`` `[`mean`](https://rdrr.io/r/base/mean.html)`(``mpg``)``)`` ``# # A tibble: 4 × 2`` ``# gear mean_mpg`` ``# <int> <dbl>`` ``# 1 3 16.1`` ``# 2 4 24.5`` ``# 3 5 21.4`` ``# 4 NA NA`` `` ``# delete documents; query is optional parameter and has to be`` ``# specified for deleting documents instead of deleting the container`` `[`dim`](https://rdrr.io/r/base/dim.html)`(`[`docdb_query`](https://docs.ropensci.org/nodbi/reference/docdb_query.md)`(``src``, ``key``, query ``=`` ``'{"$or": [{"age": {"$lte": 20}}, {"age": {"$gte": 25}}]}'``)``)`` ``# [1] 3 11`` `[`docdb_delete`](https://docs.ropensci.org/nodbi/reference/docdb_delete.md)`(``src``, ``key``, query ``=`` ``'{"$or": [{"age": {"$lte": 20}}, {"age": {"$gte": 25}}]}'``)`` ``# TRUE`` `[`nrow`](https://rdrr.io/r/base/nrow.html)`(`[`docdb_get`](https://docs.ropensci.org/nodbi/reference/docdb_get.md)`(``src``, ``key``)``)`` ``# [1] 132`` `` ``# delete container from database`` `[`docdb_delete`](https://docs.ropensci.org/nodbi/reference/docdb_delete.md)`(``src``, ``key``)`` ``# [1] TRUE`` ``#`` ``# shutdown`` ``DBI``::`[`dbDisconnect`](https://dbi.r-dbi.org/reference/dbDisconnect.html)`(``src``$``con``, shutdown ``=`` ``TRUE``)`` `[`rm`](https://rdrr.io/r/base/rm.html)`(``src``)`

## Benchmark

[`library`](https://rdrr.io/r/base/library.html)`(`[`"nodbi"`](https://docs.ropensci.org/nodbi/)`)`` `` ``srcMongo`` ``<-`` `[`src_mongo`](https://docs.ropensci.org/nodbi/reference/src_mongo.md)`(``)`` ``srcSqlite`` ``<-`` `[`src_sqlite`](https://docs.ropensci.org/nodbi/reference/src_sqlite.md)`(``)`` ``srcPostgres`` ``<-`` `[`src_postgres`](https://docs.ropensci.org/nodbi/reference/src_postgres.md)`(``)`` ``srcMariadb`` ``<-`` `[`src_mariadb`](https://docs.ropensci.org/nodbi/reference/src_mariadb.md)`(``)`` ``srcDuckdb`` ``<-`` `[`src_duckdb`](https://docs.ropensci.org/nodbi/reference/src_duckdb.md)`(``)`` ``srcElastic`` ``<-`` `[`src_elastic`](https://docs.ropensci.org/nodbi/reference/src_elastic.md)`(``)`` ``srcCouchdb`` ``<-`` `[`src_couchdb`](https://docs.ropensci.org/nodbi/reference/src_couchdb.md)`(`` `` user ``=`` `[`Sys.getenv`](https://rdrr.io/r/base/Sys.getenv.html)`(``"COUCHDB_TEST_USER"``)``,`` `` pwd ``=`` `[`Sys.getenv`](https://rdrr.io/r/base/Sys.getenv.html)`(``"COUCHDB_TEST_PWD"``)``)`` `` ``key`` ``<-`` ``"test2"`` ``query`` ``<-`` ``'{"clarity": {"$in": ["NOTME", "VS1"]}}'`` ``fields`` ``<-`` ``'{"cut": 1, "_id": 1, "clarity": 1}'`` ``value`` ``<-`` ``'{"clarity": "XYZ", "new": ["ABC", "DEF"]}'`` ``data`` ``<-`` ``diamonds``[``1``:``1000``, ``]`` ``ndjs`` ``<-`` `[`tempfile`](https://rdrr.io/r/base/tempfile.html)`(``)`` ``jsonlite``::`[`stream_out`](https://jeroen.r-universe.dev/jsonlite/reference/stream_in.html)`(``diamonds``[``1``:``10000``, ``]``, con ``=`` `[`file`](https://rdrr.io/r/base/connections.html)`(``ndjs``)``, verbose ``=`` ``FALSE``)`` `` ``testFunction`` ``<-`` ``function``(``src``)`` ``{`` `` `[`try`](https://rdrr.io/r/base/try.html)`(`[`docdb_delete`](https://docs.ropensci.org/nodbi/reference/docdb_delete.md)`(``src``, ``key``)``, silent ``=`` ``TRUE``)`` `` `[`on.exit`](https://rdrr.io/r/base/on.exit.html)`(`[`docdb_delete`](https://docs.ropensci.org/nodbi/reference/docdb_delete.md)`(``src``, ``key``)``)`` `` `[`suppressMessages`](https://rdrr.io/r/base/message.html)`(`[`docdb_create`](https://docs.ropensci.org/nodbi/reference/docdb_create.md)`(``src``, ``key``, ``data``)``)`` `` `[`suppressMessages`](https://rdrr.io/r/base/message.html)`(`[`docdb_create`](https://docs.ropensci.org/nodbi/reference/docdb_create.md)`(``src``, ``key``, ``ndjs``)``)`` `` `[`head`](https://rdrr.io/r/utils/head.html)`(`[`docdb_get`](https://docs.ropensci.org/nodbi/reference/docdb_get.md)`(``src``, ``key``)``)`` `` `[`docdb_query`](https://docs.ropensci.org/nodbi/reference/docdb_query.md)`(``src``, ``key``, query ``=`` ``query``, fields ``=`` ``fields``)`` `` `[`docdb_query`](https://docs.ropensci.org/nodbi/reference/docdb_query.md)`(``src``, ``key``, query ``=`` ``query``, listfields ``=`` ``TRUE``)`` `` `[`docdb_update`](https://docs.ropensci.org/nodbi/reference/docdb_update.md)`(``src``, ``key``, value ``=`` ``value``, query ``=`` ``query``)`` ``}`` `` ``result`` ``<-`` ``rbenchmark``::`[`benchmark`](https://rdrr.io/pkg/rbenchmark/man/benchmark.html)`(`` `` DuckDB ``=`` ``testFunction``(``src ``=`` ``srcDuckdb``)``,`` `` MariaDB ``=`` ``testFunction``(``src ``=`` ``srcMariadb``)``,`` `` MongoDB ``=`` ``testFunction``(``src ``=`` ``srcMongo``)``,`` `` PostgreSQL ``=`` ``testFunction``(``src ``=`` ``srcPostgres``)``,`` `` SQLite ``=`` ``testFunction``(``src ``=`` ``srcSqlite``)``,`` `` CouchDB ``=`` ``testFunction``(``src ``=`` ``srcCouchdb``)``,`` `` Elastic ``=`` ``testFunction``(``src ``=`` ``srcElastic``)``,`` `` replications ``=`` ``3L``,`` `` order ``=`` ``"elapsed"`` ``)`` `` ``# 2026-08-11 with M3 hardware, databases via homebrew`` ``result``[`` , `[`c`](https://rdrr.io/r/base/c.html)`(``"test"``, ``"replications"``, ``"elapsed"``)``]`` ``# test replications elapsed`` ``# 1 DuckDB 3 0.89`` ``# 5 SQLite 3 0.96`` ``# 4 PostgreSQL 3 2.12`` ``# 2 MariaDB 3 2.81`` ``# 3 MongoDB 3 3.11`` ``# 7 Elastic 3 29.0`` ``# 6 CouchDB 3 57.4`` `` `[`message`](https://rdrr.io/r/base/message.html)`(``R.version``$``version.string``)`` ``# R Under development (unstable) (2026-06-24 r90190)`` `` ``pkgs`` ``<-`` `[`c`](https://rdrr.io/r/base/c.html)`(``"nodbi"``, ``"RSQLite"``, ``"duckdb"``, ``"RPostgres"``, ``"mongolite"``, ``"elastic"``, ``"sofa"``, ``"RMariaDB"``)`` ``for`` ``(``pkg`` ``in`` ``pkgs``)`` `[`message`](https://rdrr.io/r/base/message.html)`(``pkg``, ``": "``, `[`packageVersion`](https://rdrr.io/r/utils/packageDescription.html)`(``pkg``)``)`` ``# nodbi: 0.14.0.9000`` ``# RSQLite: 3.53.3`` ``# duckdb: 1.5.5`` ``# RPostgres: 1.4.10`` ``# mongolite: 4.1.0`` ``# elastic: 1.2.2`` ``# sofa: 0.4.2`` ``# RMariaDB: 1.3.5`

## Testing

Every database backend is subject to identical tests, see
[core-nodbi.R](https://github.com/ropensci/nodbi/blob/master/tests/testthat/core-nodbi.R).

`# 2026-08-11`` `[`options`](https://rdrr.io/r/base/options.html)`(``duckdb.home ``=`` ``"~/.duckdb"``)`` `[`suppressMessages`](https://rdrr.io/r/base/message.html)`(``testthat``::`[`test_local`](https://testthat.r-lib.org/reference/test_package.html)`(``)``)`` ``# ✔ | F W S OK | Context`` ``# ✔ | 2 184 | couchdb [81.5s]`` ``# ✔ | 1 183 | duckdb [4.2s]`` ``# ✔ | 2 181 | elastic [75.0s]`` ``# ✔ | 2 181 | mariadb [7.2s]`` ``# ✔ | 2 182 | mongodb [4.8s]`` ``# ✔ | 185 | postgres [7.3s]`` ``# ✔ | 186 | sqlite [4.5s]`` `` ``covr``::`[`package_coverage`](https://rdrr.io/pkg/covr/man/package_coverage.html)`(``path ``=`` ``"."``, type ``=`` ``"tests"``)`` ``# nodbi Coverage: 94.11%`` ``# R/src_postgres.R: 82.43%`` ``# R/src_duckdb.R: 84.00%`` ``# R/zzz.R: 86.67%`` ``# R/src_mariadb.R: 92.86%`` ``# R/update.R: 94.49%`` ``# R/create.R: 94.93%`` ``# R/query.R: 94.96%`` ``# R/src_mongo.R: 95.00%`` ``# R/get.R: 96.19%`` ``# R/delete.R: 99.02%`` ``# R/exists.R: 100.00%`` ``# R/list.R: 100.00%`` ``# R/src_couchdb.R: 100.00%`` ``# R/src_elastic.R: 100.00%`` ``# R/src_sqlite.R: 100.00%`

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
