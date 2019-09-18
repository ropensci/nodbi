nodbi
=====



[![Project Status: WIP – Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](http://www.repostatus.org/badges/latest/wip.svg)](http://www.repostatus.org/#wip)
[![cran checks](https://cranchecks.info/badges/worst/nodbi)](https://cranchecks.info/pkgs/nodbi)
[![Build Status](https://travis-ci.org/ropensci/nodbi.svg)](https://travis-ci.org/ropensci/nodbi)
[![codecov](https://codecov.io/gh/ropensci/nodbi/branch/master/graph/badge.svg)](https://codecov.io/gh/ropensci/nodbi)
[![rstudio mirror downloads](http://cranlogs.r-pkg.org/badges/nodbi)](https://github.com/metacran/cranlogs.app)
[![cran version](https://www.r-pkg.org/badges/version/nodbi)](https://cran.r-project.org/package=nodbi)


`nodbi` provides a single user interface for interacting with many NoSQL databases.

So far we support the following DBs:

* MongoDB
* Redis (server based)
* CouchDB
* Elasticsearch
* SQLite

Currently we have support for data.frame's for the following operations

* Create - all DBs
* Exists - all DBs, except MongoDB
* Get - all DBs
* Query - all DBs, except Redis
* Delete - all DBs
* Update - just CouchDB

## Install

cran version


```r
install.packages("nodbi")
```

dev version


```r
install.packages("devtools")
devtools::install_github("ropensci/nodbi")
```


```r
library("nodbi")
```

## Initialize connections

Start CouchDB on the cli or with the app


```r
src_couchdb()
```

Start Elasticsearch, e.g.:

```sh
cd /usr/local/elasticsearch && bin/elasticsearch
```


```r
src_elastic()
```

If you want to use classic Redis server, we do that through the [redux][]
package, and you'll need to start up Redis by e.g,. `redis-server` in your shell.


```r
src_redis()
```

Start MongoDB: `mongod` (may need to do `sudo mongod`)


```r
src_mongo()
```

## CouchDB


```r
src <- src_couchdb()
docout <- docdb_create(src, key = "mtcars", value = mtcars)
head( docdb_get(src, "mtcars") )
```

## Elasticsearch

Put the `iris` dataset into ES




```r
src <- src_elastic()
ff <- docdb_create(src, "iris", iris)
head( docdb_get(src, "iris") )
#>   Sepal.Length Sepal.Width Petal.Length Petal.Width Species
#>          5.0         3.6          1.4         0.2  setosa
#>          4.9         3.1          1.5         0.1  setosa
#>          4.8         3.4          1.6         0.2  setosa
#>          5.4         3.9          1.3         0.4  setosa
#>          5.1         3.3          1.7         0.5  setosa
#>          5.2         3.4          1.4         0.2  setosa
```

## Redis


```r
src <- src_redis()
docdb_create(src, "mtcars", mtcars)
```


```r
docdb_get(src, "mtcars")
```

## MongoDB


```r
library("ggplot2")
src <- src_mongo(verbose = FALSE)
ff <- docdb_create(src, "diamonds", diamonds)
docdb_get(src, "diamonds")
```

## SQLite


```r
src <- src_sqlite(dbname = ":memory:")
ff <- docdb_create(src, key = "mtcars", value = mtcars)
docdb_get(src, key = "mtcars")
```

Extension [json1](https://www.sqlite.org/json1.html) is enabled in Package `RSQLite` (since version 1.1). This extension is used to emulate the behaviour of MongoDB with the methods for SQLite: 

- Parameter `collection` corresponds to the name of a table (parameter `key`) in the SQLite database `dbname`. 
- Json strings in parameters `query` and `fields` are translated into SQL commands, unless too complex. 
- Tables created by `docdb_create()` are defined as follows, with exactly two columns, an index column named `_id` and a column with json data named `json`: 
``` 
CREATE TABLE mtcars ( _id TEXT PRIMARY_KEY NOT NULL, json JSON );
CREATE UNIQUE INDEX mtcars_index ON mtcars ( _id );
```

The following examples show the maximum level of complexity that can be used at this time with available json operaters ("\$eq", "\$gt", "\$gte", "\$lt", "\$lte", "\$ne"); `query` implies AND of the comma separated expressions in the absence of a prefixed logical operator (available at this time: "\$and", "\$or"). 


```r
ff <- docdb_create(src, key = "mtcars", value = contacts)
docdb_query(src, "mtcars", 
            query = '{"$or": {"eyeColor": "blue", 
                              "age": {"$lt": 22}},  
                              "name": {"$regex": "L%"} }',
            fields = '{"age": 1, "eyeColor": 1, "name": 1}')
```


## Use with dplyr


```r
library("dplyr")
src <- src_mongo(verbose = FALSE)
```


```r
docdb_get(src, "diamonds") %>%
  group_by(cut) %>%
  summarise(mean_depth = mean(depth), mean_price = mean(price))
```

## Meta

* Please [report any issues or bugs](https://github.com/ropensci/nodbi/issues).
* License: MIT
* Get citation information for `nodbi` in R doing `citation(package = 'nodbi')`
* Please note that this project is released with a [Contributor Code of Conduct][coc].
By participating in this project you agree to abide by its terms.

[![rofooter](https://ropensci.org/public_images/github_footer.png)](https://ropensci.org)

[redux]: https://cran.r-project.org/package=redux
[coc]: https://github.com/ropensci/nodbi/blob/master/CODE_OF_CONDUCT.md
