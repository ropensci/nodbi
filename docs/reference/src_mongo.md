# Setup a MongoDB database connection

Setup a MongoDB database connection

## Usage

``` r
src_mongo(collection = "test", db = "test", url = "mongodb://localhost", ...)
```

## Arguments

- collection:

  (character) Name of collection

- db:

  (character) Name of database

- url:

  (character) Address of the MongoDB server in Mongo connection string
  URI format, see to
  [`mongolite::mongo()`](https://jeroen.r-universe.dev/mongolite/reference/mongo.html)

- ...:

  Additional named parameters passed on to
  [`mongolite::mongo()`](https://jeroen.r-universe.dev/mongolite/reference/mongo.html)

## Value

A `nodbi` source object

## Details

Uses monoglite as backend. nodbi creates or uses a MongoDB collection,
in which `nodbi` creates JSON documents. If documents do not have
root-level `_id`'s, UUID's are created as `_id`'s. MongoDB but none of
the other databases require to specify the container already in the
`src_mongo()` function. For a benchmark, see
<https://github.com/ropensci/nodbi#benchmark>

## Examples

``` r
if (FALSE) { # \dontrun{
con <- src_mongo()
print(con)
} # }
```
