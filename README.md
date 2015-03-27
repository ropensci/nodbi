docdbi
========



## Install


```r
install.packages("devtools")
devtools::install_github("ropensci/docdbi")
```


```r
library("docdbi")
```

## CouchDB


```r
src <- src_couchdb()
docout <- docdb_create(src, key = "mtcars", value = mtcars)
docdb_get(src, "mtcars")
#>       mpg cyl  disp  hp drat    wt  qsec vs am gear carb
#>   1: 21.0   6 160.0 110 3.90 2.620 16.46  0  1    4    4
#>   2: 21.0   6 160.0 110 3.90 2.875 17.02  0  1    4    4
#>   3: 22.8   4 108.0  93 3.85 2.320 18.61  1  1    4    1
#>   4: 21.4   6 258.0 110 3.08 3.215 19.44  1  0    3    1
#>   5: 18.7   8 360.0 175 3.15 3.440 17.02  0  0    3    2
#>  ---                                                    
#> 188: 30.4   4  95.1 113 3.77 1.513 16.90  1  1    5    2
#> 189: 15.8   8 351.0 264 4.22 3.170 14.50  0  1    5    4
#> 190: 19.7   6 145.0 175 3.62 2.770 15.50  0  1    5    6
#> 191: 15.0   8 301.0 335 3.54 3.570 14.60  0  1    5    8
#> 192: 21.4   4 121.0 109 4.11 2.780 18.60  1  1    4    2
```

## Redis


```r
docdb_src_rrlite()
#> src: rrlite 0.1.0 [:memory:]
```

## etcd 


```r
src <- src_etcd()
ff <- docdb_create(src, "hi", "there")
docdb_get(src, "hi")
#> $action
#> [1] "get"
#> 
#> $node
#> $node$key
#> [1] "/hi"
#> 
#> $node$value
#> [1] "there"
#> 
#> $node$modifiedIndex
#> [1] 95
#> 
#> $node$createdIndex
#> [1] 95
```

## Meta

* [Please report any issues or bugs](https://github.com/ropensci/docdbi/issues).
* License: MIT
* Get citation information for `docdbi` in R doing `citation(package = 'docdbi')`

[![rofooter](http://ropensci.org/public_images/github_footer.png)](http://ropensci.org)
