docdbi
======



## Install


```r
install.packages("devtools")
devtools::install_github("ropensci/docdbi")
```


```r
library("docdbi")
```

## Initialize connections


```r
docdb_src_rrlite()
```

```
#> src: rrlite 0.1.0 [:memory:]
```

```r
src_couchdb()
```

```
#> src: couchdb 1.6.1 [localhost/5984]
#> databases: _replicator, _users, adsfa, adsfdsf, bulkfromchr, bulkfromlist,
#>      bulktest, bulktest2, bulktest3, bulktest4, bulktest5, cachecall, diamonds,
#>      docdbi, hello_earth, iris, iriscolumns, irisrows, leothelion, leothelion2,
#>      mapuris, mran, mtcars, mtcars2, mtcarsdb, mydb, newdb, newdbs, newnew,
#>      stuff, stuff2, test
```

```r
src_elasticsearch()
```

```
#> src: elasticsearch 1.5.0 [http://127.0.0.1:9200]
#> databases: stuff, geonames, gbif, mtcars, twitter, iris, gbifgeopoint,
#>      gbifnewgeo, logstash-2018.02.28, plos, shakespeare, things2, gbifgeo,
#>      shakespeare2, geoshape, diamonds_small, things
```

```r
src_etcd()
```

```
#> src: etcd 0.4.6
```

## CouchDB


```r
src <- src_couchdb()
docout <- docdb_create(src, key = "mtcars", value = mtcars)
head( docdb_get(src, "mtcars") )
#>     mpg cyl disp  hp drat    wt  qsec vs am gear carb
#> 1: 21.0   6  160 110 3.90 2.620 16.46  0  1    4    4
#> 2: 21.0   6  160 110 3.90 2.875 17.02  0  1    4    4
#> 3: 22.8   4  108  93 3.85 2.320 18.61  1  1    4    1
#> 4: 21.4   6  258 110 3.08 3.215 19.44  1  0    3    1
#> 5: 18.7   8  360 175 3.15 3.440 17.02  0  0    3    2
#> 6: 18.1   6  225 105 2.76 3.460 20.22  1  0    3    1
```

## etcd 




```r
src <- src_etcd()
ff <- docdb_create(src, "/mtcars", mtcars)
head( docdb_get(src, "/mtcars") )
#>     mpg cyl  disp  hp drat    wt  qsec vs am gear carb
#> 1: 32.4   4  78.7  66 4.08 2.200 19.47  1  1    4    1
#> 2: 33.9   4  71.1  65 4.22 1.835 19.90  1  1    4    1
#> 3: 19.2   8 400.0 175 3.08 3.845 17.05  0  0    3    2
#> 4: 26.0   4 120.3  91 4.43 2.140 16.70  0  1    5    2
#> 5: 18.1   6 225.0 105 2.76 3.460 20.22  1  0    3    1
#> 6: 24.4   4 146.7  62 3.69 3.190 20.00  1  0    4    2
```

## Elasticsearch

Put the `iris` dataset into ES




```r
src <- src_elasticsearch()
ff <- docdb_create(src, "iris", iris)
head( docdb_get(src, "iris") )
```

Put part of the `diamonds` dataset into ES




```r
library("ggplot2")
ff <- docdb_create(src, "diamonds_small", diamonds[1:1000,])
head( docdb_get(src, "diamonds_small") )
```

## Redis


```r
docdb_src_rrlite()
#> src: rrlite 0.1.0 [:memory:]
```

## Use with dplyr


```r
library("dplyr")
src <- src_elasticsearch()
```


```r
docdb_get(src, "iris") %>% 
  group_by(Species) %>% 
  summarise(mean = mean(Petal.Length))
#> Source: local data table [3 x 2]
#> 
#>      Species  mean
#> 1     setosa 1.462
#> 2 versicolor 4.260
#> 3  virginica 5.552
```


```r
docdb_get(src, "diamonds_small") %>% 
  group_by(cut) %>% 
  summarise(mean_depth = mean(depth), mean_price = mean(price))
#> Source: local data table [5 x 3]
#> 
#>         cut mean_depth mean_price
#> 1      Good   62.38090   2185.303
#> 2 Very Good   61.74602   2365.310
#> 3     Ideal   61.67477   2503.051
#> 4   Premium   61.21793   2554.372
#> 5      Fair   63.31290   2793.613
```

## Meta

* [Please report any issues or bugs](https://github.com/ropensci/docdbi/issues).
* License: MIT
* Get citation information for `docdbi` in R doing `citation(package = 'docdbi')`

[![rofooter](http://ropensci.org/public_images/github_footer.png)](http://ropensci.org)
