nodbi
=====

```{r echo=FALSE}
library("knitr")
hook_output <- knitr::knit_hooks$get("output")
knitr::knit_hooks$set(output = function(x, options) {
   lines <- options$output.lines
   if (is.null(lines)) {
     return(hook_output(x, options))  # pass to default hook
   }
   x <- unlist(strsplit(x, "\n"))
   more <- "..."
   if (length(lines)==1) {        # first n lines
     if (length(x) > lines) {
       # truncate the output, but add ....
       x <- c(head(x, lines), more)
     }
   } else {
     x <- c(if (abs(lines[1])>1) more else NULL,
            x[lines],
            if (length(x)>lines[abs(length(lines))]) more else NULL
           )
   }
   # paste these lines together
   x <- paste(c(x, ""), collapse = "\n")
   hook_output(x, options)
})

knitr::opts_chunk$set(
  comment = "#>",
  collapse = TRUE,
  warning = FALSE,
  message = FALSE
)
```


`nodbi` provides a single UI for interacting with many NoSQL databases. 

So far we support the following DBs:

* MongoDB
* Redis (server and serverless)
* CouchDB
* Elasticsearch
* etcd
* Riak

Currently we have support for data.frame's for the following operations

* Create - all DBs
* Get - all DBs
* Delete - all DBs
* Update - just CouchDB (others coming)

`sofa`, `mongolite`, `elastic`, and `etseed` are on CRAN

`RedisAPI`, `rrlite`, `reeack` are not on CRAN

## Install

```{r eval=FALSE}
install.packages("devtools")
devtools::install_github("ropensci/nodbi")
```

```{r}
library("nodbi")
```

## Initialize connections

Start CouchDB in your shell of choice by, e.g.: `couchdb`

```{r}
src_couchdb()
```

Start Elaticsearch in your shell of choice by, e.g.:

```sh
cd /usr/local/elasticsearch && bin/elasticsearch
```

```{r}
src_elasticsearch()
```

Start etcd in your shell of choice after installing etcd (https://github.com/coreos/etcd/releases/tag/v2.2.0) by, e.g.: `etcd`

```{r}
src_etcd()
```

Start MongoDB in your shell of choice by: `mongod`

```{r}
src_mongo()
```

If you want to use classic Redis server, we do that through the [RedisAPi][redisapi] 
package, and you'll need to start up Redis by e.g,. `redis-server` in your shell. 

```{r output.lines=1:10}
src_redis()
```

But if you want to use serverless Redis via [rlite][rlite], we do that through using 
with the [rrlite][rrlite] R package - and no need to start a server, of course.

```{r output.lines=1:10}
src_rlite()
```

Start your Riak server, then:

```{r}
src_riak()
```

## CouchDB

```{r}
src <- src_couchdb()
docout <- docdb_create(src, key = "mtcars", value = mtcars)
head( docdb_get(src, "mtcars") )
```

## etcd

```{r echo=FALSE}
src <- src_etcd()
invisible(docdb_delete(src, "/mtcars"))
```

```{r}
src <- src_etcd()
ff <- docdb_create(src, "/mtcars", mtcars)
head( docdb_get(src, "/mtcars") )
```

## Elasticsearch

Put the `iris` dataset into ES

```{r echo=FALSE}
src <- src_elasticsearch()
if (elastic::index_exists("iris")) invisible(docdb_delete(src, "iris"))
```

```{r eval=FALSE}
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

```{r output.lines=1:10}
library("ggplot2")
src <- src_mongo(verbose = FALSE)
ff <- docdb_create(src, "diamonds", diamonds)
docdb_get(src, "diamonds")
```

## Redis

```{r}
src <- src_rlite()
docdb_create(src, "mtcars", mtcars)
```

```{r output.lines=1:10}
docdb_get(src, "mtcars")
```

## Riak

```{r echo=FALSE}
src <- src_riak()
invisible(docdb_delete(src, "iris"))
```

```{r}
src <- src_riak()
docdb_create(src, "iris", iris)
```


## Use with dplyr

```{r}
library("dplyr")
src <- src_mongo(verbose = FALSE)
```

```{r}
docdb_get(src, "diamonds") %>%
  group_by(cut) %>%
  summarise(mean_depth = mean(depth), mean_price = mean(price))
```

## Meta

* Please [report any issues or bugs](https://github.com/ropensci/nodbi/issues).
* License: MIT
* Get citation information for `nodbi` in R doing `citation(package = 'nodbi')`
* Please note that this project is released with a [Contributor Code of Conduct](CONDUCT.md). By participating in this project you agree to abide by its terms.

[![rofooter](https://ropensci.org/public_images/github_footer.png)](https://ropensci.org)

[rlite]: https://github.com/seppo0010/rlite
[redisapi]: https://github.com/ropensci/RedisAPI
[rrlite]: https://github.com/ropensci/rrlite
