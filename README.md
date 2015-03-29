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
#> 1: 21.0   6 160.0 110 3.90 2.875 17.02  0  1    4    4
#> 2: 15.2   8 275.8 180 3.07 3.780 18.00  0  0    3    3
#> 3: 32.4   4  78.7  66 4.08 2.200 19.47  1  1    4    1
#> 4: 13.3   8 350.0 245 3.73 3.840 15.41  0  0    3    4
#> 5: 22.8   4 108.0  93 3.85 2.320 18.61  1  1    4    1
#> 6: 18.7   8 360.0 175 3.15 3.440 17.02  0  0    3    2
```

## Redis


```r
docdb_src_rrlite()
#> src: rrlite 0.1.0 [:memory:]
```

## Meta

* [Please report any issues or bugs](https://github.com/ropensci/docdbi/issues).
* License: MIT
* Get citation information for `docdbi` in R doing `citation(package = 'docdbi')`

[![rofooter](http://ropensci.org/public_images/github_footer.png)](http://ropensci.org)
