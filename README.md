nodbi
======




`nodbi` provides a single UI for interacting with many NoSQL databases. 

So far we support the following DBs:

* MongoDB
* Redis (server and serverless)
* CouchDB
* Elasticsearch
* etcd

Currently we have support for data.frame's for the following operations

* Create - all DBs
* Get - all DBs
* Delete - all DBs
* Update - just CouchDB (others coming)

## Dependencies not on CRAN

You don't need to install these, only the ones that you want to use in `nodbi`.
We put all database drivers in `Suggests`, so this package doesn't require
any one database drive package unless you invoke the function that uses that 
thing. 


```r
install.packages("devtools")
devtools::install_github("ropensci/sofa")
devtools::install_github("ropensci/etseed")
devtools::install_github("ropensci/rrlite")
devtools::install_github("ropensci/RedisAPI")
```

`mongolite` and `elastic` are on CRAN

## Install


```r
install.packages("devtools")
devtools::install_github("ropensci/nodbi")
```


```r
library("nodbi")
```

## Initialize connections

Start CouchDB in your shell of choice by, e.g.: `couchdb`


```r
src_couchdb()
#> src: couchdb 1.6.1 [localhost/5984]
#> databases: _replicator, _users, adsfa, adsfdsf, bulkfromchr, bulkfromlist,
#>      bulktest, bulktest2, bulktest3, bulktest4, bulktest5, cachecall, diamonds,
#>      docdbi, hello_earth, iris, iriscolumns, irisrows, leothelion, leothelion2,
#>      mapuris, mran, mtcars2, mtcars3, mtcarsdb, mydb, newdb, newdbs, newnew,
#>      sofadb, stuff, stuff2, test, testiris, xiris
```

Start Elaticsearch in your shell of choice by, e.g.:

```sh
cd /usr/local/elasticsearch && bin/elasticsearch
```


```r
src_elasticsearch()
#> src: elasticsearch 1.7.2 [http://127.0.0.1:9200]
#> databases: flowers, shite, animals, asdfdf, things2, twitter, testrgdal, -----,
#>      arrests, flights, testlist, diam, logstash-2018.02.28, stuff, bbbbbbb,
#>      gbif, afjaljfalsfjalksdfadf, stuff_m, testes, gbifnewgeo, geoshape,
#>      stuff_x, afjaljfalsfjalksdf, diamfromlist, diamonds, stuff_i, pos,
#>      shakespeare2, stuff_e, iris_data, stuff_g, geonames, gbifgeo, yep,
#>      diamonds_small, foobar, stuff_k, things, shakespeare, stuff_j,
#>      gbifgeopoint, stuff_w, hello
```

Start etcd in your shell of choice after installing etcd (https://github.com/coreos/etcd/releases/tag/v2.2.0) by, e.g.: `etcd`


```r
src_etcd()
#> src:
#>   etcd server: 2.2.0
#>   etcd cluster: 2.2.0
```

Start MongoDB in your shell of choice by: `mongod`


```r
src_mongo()
#> MongoDB 3.0.5 (uptime: 141s)
#> URL: Scotts-MBP/test
```

If you want to use classic Redis server, we do that through the [RedisAPi][redisapi] 
package, and you'll need to start up Redis by e.g,. `redis-server` in your shell. 


```r
src_redis()
#> $type
#> [1] "redis"
#> 
#> $version
#> [1] '0.1.4'
#> 
#> $con
#> <redis_api>
#>   Public:
#>     APPEND: function
...
```

But if you want to use serverless Redis via [rlite][rlite], we do that through using 
with the [rrlite][rrlite] R package - and no need to start a server, of course.


```r
src_rlite()
#> $type
#> [1] "redis"
#> 
#> $version
#> [1] '0.2.0'
#> 
#> $con
#> <redis_api>
#>   Public:
#>     APPEND: function
...
```

## CouchDB


```r
src <- src_couchdb()
docout <- docdb_create(src, key = "mtcars", value = mtcars)
head( docdb_get(src, "mtcars") )
#>     mpg cyl disp  hp drat    wt  qsec vs am gear carb letter
#> 1: 21.0   6  160 110 3.90 2.620 16.46  0  1    4    4      n
#> 2: 21.0   6  160 110 3.90 2.875 17.02  0  1    4    4      o
#> 3: 22.8   4  108  93 3.85 2.320 18.61  1  1    4    1      y
#> 4: 21.4   6  258 110 3.08 3.215 19.44  1  0    3    1      d
#> 5: 18.7   8  360 175 3.15 3.440 17.02  0  0    3    2      q
#> 6: 18.1   6  225 105 2.76 3.460 20.22  1  0    3    1      u
```

## etcd




```r
src <- src_etcd()
ff <- docdb_create(src, "/mtcars", mtcars)
head( docdb_get(src, "/mtcars") )
#>     mpg cyl disp  hp drat    wt  qsec vs am gear carb letter
#> 1: 21.0   6  160 110 3.90 2.620 16.46  0  1    4    4      n
#> 2: 21.0   6  160 110 3.90 2.875 17.02  0  1    4    4      o
#> 3: 22.8   4  108  93 3.85 2.320 18.61  1  1    4    1      y
#> 4: 21.4   6  258 110 3.08 3.215 19.44  1  0    3    1      d
#> 5: 18.7   8  360 175 3.15 3.440 17.02  0  0    3    2      q
#> 6: 18.1   6  225 105 2.76 3.460 20.22  1  0    3    1      u
```

## Elasticsearch

Put the `iris` dataset into ES




```r
src <- src_elasticsearch()
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

## MongoDB


```r
library("ggplot2")
src <- src_mongo()
ff <- docdb_create(src, "diamonds_small", diamonds)
docdb_get(src, "diamonds_small")
#>        carat       cut color clarity depth table price     x     y     z
#> 1       0.23     Ideal     E     SI2  61.5  55.0   326  3.95  3.98  2.43
#> 2       0.21   Premium     E     SI1  59.8  61.0   326  3.89  3.84  2.31
#> 3       0.23      Good     E     VS1  56.9  65.0   327  4.05  4.07  2.31
#> 4       0.29   Premium     I     VS2  62.4  58.0   334  4.20  4.23  2.63
#> 5       0.31      Good     J     SI2  63.3  58.0   335  4.34  4.35  2.75
#> 6       0.24 Very Good     J    VVS2  62.8  57.0   336  3.94  3.96  2.48
#> 7       0.24 Very Good     I    VVS1  62.3  57.0   336  3.95  3.98  2.47
#> 8       0.26 Very Good     H     SI1  61.9  55.0   337  4.07  4.11  2.53
#> 9       0.22      Fair     E     VS2  65.1  61.0   337  3.87  3.78  2.49
...
```

## Redis


```r
src <- src_rlite()
docdb_create(src, "mtcars", mtcars)
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

## Use with dplyr


```r
library("dplyr")
src <- src_elasticsearch()
```


```r
docdb_get(src, "diamonds_small") %>%
  group_by(cut) %>%
  summarise(mean_depth = mean(depth), mean_price = mean(price))
#> Source: local data table [5 x 3]
#> 
#>         cut mean_depth mean_price
#>       (chr)      (dbl)      (dbl)
#> 1      Good   62.38090   2185.303
#> 2 Very Good   61.74602   2365.310
#> 3     Ideal   61.67477   2503.051
#> 4   Premium   61.21793   2554.372
#> 5      Fair   63.31290   2793.613
```

## Meta

* [Please report any issues or bugs](https://github.com/ropensci/nodbi/issues).
* License: MIT
* Get citation information for `nodbi` in R doing `citation(package = 'nodbi')`
* Please note that this project is released with a [Contributor Code of Conduct](CONDUCT.md). By participating in this project you agree to abide by its terms.

[![rofooter](http://ropensci.org/public_images/github_footer.png)](http://ropensci.org)

[rlite]: https://github.com/seppo0010/rlite
[redisapi]: https://github.com/ropensci/RedisAPI
[rrlite]: https://github.com/ropensci/rrlite
