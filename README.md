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
* etcd

Currently we have support for data.frame's for the following operations

* Create - all DBs
* Exists - except MongoDB
* Get - all DBs
* Query - except Redis, etcd
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
#> src: couchdb 2.3.0 [127.0.0.1/5984]
#> databases: cats, df, flights, foobar, geotest, mtcars, mtcars2, sofadb, test,
#>      testing123
```

Start Elasticsearch, e.g.:

```sh
cd /usr/local/elasticsearch && bin/elasticsearch
```


```r
src_elastic()
#> src: elasticsearch 7.0.0 [127.0.0.1:9200]
#> databases: gbifgeo, mtcars, gbif, plos, diamonds_small
```

Start etcd after installing etcd (https://github.com/coreos/etcd/releases) by, e.g.: `etcd`


```r
src_etcd()
#> src:
#>   etcd server: 3.3.11
#>   etcd cluster: 3.3.0
```

If you want to use classic Redis server, we do that through the [redux][]
package, and you'll need to start up Redis by e.g,. `redis-server` in your shell.


```r
src_redis()
#> src: redis 1.1.0 [127.0.0.1:6379]
#> keys: diamonds, mtcars, foo
```

Start MongoDB: `mongod` (may need to do `sudo mongod`)


```r
src_mongo()
#> MongoDB 4.0.5 (uptime: 2300s)
#> URL: leothelion.local/test
```

## CouchDB


```r
src <- src_couchdb()
docout <- docdb_create(src, key = "mtcars", value = mtcars)
head( docdb_get(src, "mtcars") )
#>    mpg cyl disp  hp drat    wt  qsec vs am gear carb
#> 1 21.0   6  160 110 3.90 2.620 16.46  0  1    4    4
#> 2 21.0   6  160 110 3.90 2.875 17.02  0  1    4    4
#> 3 22.8   4  108  93 3.85 2.320 18.61  1  1    4    1
#> 4 21.4   6  258 110 3.08 3.215 19.44  1  0    3    1
#> 5 18.7   8  360 175 3.15 3.440 17.02  0  0    3    2
#> 6 18.1   6  225 105 2.76 3.460 20.22  1  0    3    1
```

## etcd




```r
src <- src_etcd()
ff <- docdb_create(src, "/mtcars", mtcars)
head( docdb_get(src, "/mtcars") )
#>    mpg cyl disp  hp drat    wt  qsec vs am gear carb
#> 1 21.0   6  160 110 3.90 2.620 16.46  0  1    4    4
#> 2 21.0   6  160 110 3.90 2.875 17.02  0  1    4    4
#> 3 22.8   4  108  93 3.85 2.320 18.61  1  1    4    1
#> 4 21.4   6  258 110 3.08 3.215 19.44  1  0    3    1
#> 5 18.7   8  360 175 3.15 3.440 17.02  0  0    3    2
#> 6 18.1   6  225 105 2.76 3.460 20.22  1  0    3    1
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
#> [Redis: OK]
```


```r
docdb_get(src, "mtcars")
#>                      mpg cyl  disp  hp drat    wt  qsec vs am gear carb
#> Mazda RX4           21.0   6 160.0 110 3.90 2.620 16.46  0  1    4    4
#> Mazda RX4 Wag       21.0   6 160.0 110 3.90 2.875 17.02  0  1    4    4
#> Datsun 710          22.8   4 108.0  93 3.85 2.320 18.61  1  1    4    1
#> Hornet 4 Drive      21.4   6 258.0 110 3.08 3.215 19.44  1  0    3    1
#> Hornet Sportabout   18.7   8 360.0 175 3.15 3.440 17.02  0  0    3    2
#> Valiant             18.1   6 225.0 105 2.76 3.460 20.22  1  0    3    1
#> Duster 360          14.3   8 360.0 245 3.21 3.570 15.84  0  0    3    4
#> Merc 240D           24.4   4 146.7  62 3.69 3.190 20.00  1  0    4    2
#> Merc 230            22.8   4 140.8  95 3.92 3.150 22.90  1  0    4    2
...
```

## MongoDB


```r
library("ggplot2")
src <- src_mongo(verbose = FALSE)
ff <- docdb_create(src, "diamonds", diamonds)
docdb_get(src, "diamonds")
#>                      mpg cyl  disp  hp drat    wt  qsec vs am gear carb
#> Mazda RX4           21.0   6 160.0 110 3.90 2.620 16.46  0  1    4    4
#> Mazda RX4 Wag       21.0   6 160.0 110 3.90 2.875 17.02  0  1    4    4
#> Datsun 710          22.8   4 108.0  93 3.85 2.320 18.61  1  1    4    1
#> Hornet 4 Drive      21.4   6 258.0 110 3.08 3.215 19.44  1  0    3    1
#> Hornet Sportabout   18.7   8 360.0 175 3.15 3.440 17.02  0  0    3    2
#> Valiant             18.1   6 225.0 105 2.76 3.460 20.22  1  0    3    1
#> Duster 360          14.3   8 360.0 245 3.21 3.570 15.84  0  0    3    4
#> Merc 240D           24.4   4 146.7  62 3.69 3.190 20.00  1  0    4    2
#> Merc 230            22.8   4 140.8  95 3.92 3.150 22.90  1  0    4    2
...
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
#> # A tibble: 6 x 3
#>   cut       mean_depth mean_price
#>   <chr>          <dbl>      <dbl>
#> 1 <NA>            NA          NA 
#> 2 Fair            64.0      4359.
#> 3 Good            62.4      3929.
#> 4 Ideal           61.7      3458.
#> 5 Premium         61.3      4584.
#> 6 Very Good       61.8      3982.
```

## Meta

* Please [report any issues or bugs](https://github.com/ropensci/nodbi/issues).
* License: MIT
* Get citation information for `nodbi` in R doing `citation(package = 'nodbi')`
* Please note that this project is released with a [Contributor Code of Conduct](CODE_OF_CONDUCT.md). By participating in this project you agree to abide by its terms.

[![rofooter](https://ropensci.org/public_images/github_footer.png)](https://ropensci.org)

[redux]: https://cran.r-project.org/package=redux
