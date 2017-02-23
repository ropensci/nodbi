
```r
knitr::opts_chunk$set(
  comment = "#>",
  collapse = TRUE,
  warning = FALSE,
  message = FALSE
)
```

# Benchmarking nodbi


```r
library(nodbi)
library(microbenchmark)
```

make connections for use below


```r
src_m <- src_mongo()
src_red <- src_redis()
src_rl <- src_rlite()
src_c <- src_couchdb()
src_e <- src_elasticsearch()
src_ri <- src_riak()
#src_et <- src_etcd()
```

delete any datasets to be used below

to do ...


## initialize connection


```r
microbenchmark(
  mongo = src_mongo(),
  redis = src_redis(),
  rlite = src_rlite(),
  couch = src_couchdb(),
  elasticsearch = src_elasticsearch(),
  riak = src_riak(),
  etcd = src_etcd(),
  times = 100
)
#> Unit: milliseconds
#>           expr       min        lq      mean    median        uq       max
#>          mongo  1.082408  1.294793  2.040935  1.416415  1.705144  17.53605
#>          redis  2.497329  2.859878  4.767820  3.287540  4.492701  46.12102
#>          rlite  1.745421  2.008409  2.895608  2.190136  2.778588  18.95398
#>          couch 12.900579 14.179201 30.828498 16.013078 20.005475 321.11350
#>  elasticsearch 12.998076 14.447466 23.544338 17.785322 22.305072 203.46777
#>           riak 23.888188 26.996847 40.102676 29.710338 36.934067 185.78502
#>           etcd  5.672403  6.093102 11.164707  6.591032  9.857925 100.61295
#>  neval
#>    100
#>    100
#>    100
#>    100
#>    100
#>    100
#>    100
```

## create


```r
microbenchmark(
  mongo = docdb_create(src_m, paste0("iris", sample(1:1000, 1)), iris),
  redis = docdb_create(src_red, paste0("iris", sample(1:1000, 1)), iris),
  rlite = docdb_create(src_rl, paste0("iris", sample(1:1000, 1)), iris),
  couch = docdb_create(src_c, paste0("iris", sample(1:1000, 1)), iris),
  elasticsearch = docdb_create(src_e, paste0("iris", sample(1:1000, 1)), iris),
  riak = docdb_create(src_ri, paste0("iris", sample(1:1000, 1)), iris),
  #etcd = docdb_create(src_et, paste0("/iris", sample(1:1000, 1)), iris),
  times = 10
)
#> Complete! Processed total of 150 rows.
#> Complete! Processed total of 150 rows.
#> Error: length(attr(src, "dbs")) == 1 is not TRUE
```

## get

create some data that won't be affected by above


```r
try_del_create <- function(src, key) {
  invisible(tryCatch(docdb_delete(src, key), error = function(e) e))
  invisible(docdb_create(src, key, iris))
}
```


```r
try_del_create(src_m, "iris_get")
#> Complete! Processed total of 150 rows.
try_del_create(src_red, "iris_get")
try_del_create(src_rl, "iris_get")
try_del_create(src_c, "iris_get")
try_del_create(src_e, "iris_get")
#>   |                                                                         |                                                                 |   0%  |                                                                         |=================================================================| 100%
try_del_create(src_ri, "iris_get")
#> Error: length(attr(src, "dbs")) == 1 is not TRUE
#try_del_create(src_et, "/iris_get")
```

benchmark


```r
microbenchmark(
  mongo = docdb_get(src_m, "iris_get"),
  redis = docdb_get(src_red, "iris_get"),
  rlite = docdb_get(src_rl, "iris_get"),
  couch = docdb_get(src_c, "iris_get"),
  elasticsearch = docdb_get(src_e, "iris_get"),
  riak = docdb_get(src_ri, "iris_get"),
  #etcd = docdb_get(src_et, "/iris_get"),
  times = 100
)
#> Error: Not Found (HTTP 404).
```

## delete

to do ...

## cleanup


```r
docdb_delete(src_m, "iris_get")
#> [1] TRUE
docdb_delete(src_red, "iris_get")
#> [1] 1
docdb_delete(src_rl, "iris_get")
#> [1] 1
docdb_delete(src_c, "iris_get")
#> $ok
#> [1] TRUE
docdb_delete(src_e, "iris_get")
#> $acknowledged
#> [1] TRUE
docdb_delete(src_ri, "iris_get")
#> Error: Not Found (HTTP 404).
#docdb_delete(src_et, "/iris_get")
```
