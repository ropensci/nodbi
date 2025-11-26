# Setup an Elasticsearch database connection

Setup an Elasticsearch database connection

## Usage

``` r
src_elastic(
  host = "127.0.0.1",
  port = 9200,
  path = NULL,
  transport_schema = "http",
  user = NULL,
  pwd = NULL,
  force = FALSE,
  ...
)
```

## Arguments

- host:

  (character) the base url, defaults to localhost (http://127.0.0.1)

- port:

  (character) port to connect to, defaults to 9200 (optional)

- path:

  (character) context path that is appended to the end of the url.
  Default: `NULL`, ignored

- transport_schema:

  (character) http or https. Default: http

- user:

  (character) User name, if required for the connection. You can
  specify, but ignored for now.

- pwd:

  (character) Password, if required for the connection. You can specify,
  but ignored for now.

- force:

  (logical) Force re-load of connection details

- ...:

  Further args passed on to
  [`elastic::connect()`](https://docs.ropensci.org/elastic/reference/connect.html)

## Value

A `nodbi` source object

## Details

Uses elastic as backend. nodbi creates or uses an Elasticsearch index,
in which `nodbi` creates JSON documents. Any root-level `_id` is
extracted from the document(s) and used as document ID `_id`, otherwise
a UUID is created as document ID `_id`. Only lowercase is accepted for
container names (in parameter `key`). Opensearch can equally be used.
For a benchmark, see <https://github.com/ropensci/nodbi#benchmark>

## Examples

``` r
if (FALSE) { # \dontrun{
con <- src_elastic()
print(con)
} # }
```
