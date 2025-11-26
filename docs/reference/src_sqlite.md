# Setup a RSQLite database connection

Setup a RSQLite database connection

## Usage

``` r
src_sqlite(dbname = ":memory:", ...)
```

## Arguments

- dbname:

  (character) name of database file, defaults to ":memory:" for an
  in-memory database, see
  [`RSQLite::SQLite()`](https://rsqlite.r-dbi.org/reference/SQLite.html)

- ...:

  additional named parameters passed on to
  [`RSQLite::SQLite()`](https://rsqlite.r-dbi.org/reference/SQLite.html)

## Value

A `nodbi` source object

## Details

Uses RSQLite as backend. nodbi creates or uses an SQLite table, with
columns `_id` and `json` created and used by package `nodbi`, applying
SQL functions as per <https://www.sqlite.org/json1.html> to the `json`
column. Each row in the table represents a `JSON` document. Any
root-level `_id` is extracted from the document(s) and used for column
`_id`, otherwise a UUID is created as `_id`. The table is indexed on
`_id`. For a benchmark, see
<https://github.com/ropensci/nodbi#benchmark>

## Examples

``` r
if (FALSE) { # \dontrun{
con <- src_sqlite()
print(con)
} # }
```
