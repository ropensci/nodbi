# Setup a CouchDB database connection

Setup a CouchDB database connection

## Usage

``` r
src_couchdb(
  host = "127.0.0.1",
  port = 5984,
  path = NULL,
  transport = "http",
  user = NULL,
  pwd = NULL,
  headers = NULL
)
```

## Arguments

- host:

  (character) host value, default: 127.0.0.1

- port:

  (integer/numeric) Port. Remember that if you don't want a port set,
  set this parameter to NULL. Default: 5984

- path:

  (character) context path that is appended to the end of the url, e.g.,
  bar in http://foo.com/bar. Default: NULL, ignored

- transport:

  (character) http or https. Default: http

- user:

  (character) Username, if any

- pwd:

  (character) Password, if any

- headers:

  (list) list of named headers

## Value

A `nodbi` source object

## Details

Uses sofa as backend. nodbi creates or uses a CouchDB database with JSON
documents. If documents do not have root-level `_id`'s, UUID's are
created as `_id`'s. Function
[`docdb_update()`](https://docs.ropensci.org/nodbi/reference/docdb_update.md)
uses [`jqr::jqr()`](https://docs.ropensci.org/jqr/reference/jqr.html) to
implement patching JSON. For a benchmark, see
<https://github.com/ropensci/nodbi#benchmark>.

## Examples

``` r
if (FALSE) { # \dontrun{
con <- src_couchdb()
print(con)
} # }
```
