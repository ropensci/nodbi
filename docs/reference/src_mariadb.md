# Setup a RMariaDB database connection

Setup a RMariaDB database connection

## Usage

``` r
src_mariadb(dbname = "test", ...)
```

## Arguments

- dbname:

  (character) name of database file, defaults to ":memory:" for an
  in-memory database, see
  [`RMariaDB::MariaDB()`](https://rmariadb.r-dbi.org/reference/dbConnect-MariaDBDriver-method.html)

- ...:

  additional named parameters passed on to
  [`RMariaDB::MariaDB()`](https://rmariadb.r-dbi.org/reference/dbConnect-MariaDBDriver-method.html)

## Value

A `nodbi` source object

## Details

Uses RSMariaDB as backend; requires MariaDB 12.3 or later. nodbi creates
or uses a table with columns `_id` and `json`, created and used by
package `nodbi`, applying SQL or JSON functions as per
https://mariadb.com/docs/server/reference/sql-functions/special-functions/json-functions
to the `json` column. Each row in the table represents a `JSON`
document. Any root-level `_id` is extracted from the document(s) and
used for column `_id`, otherwise a UUID is created as `_id`. The table
is indexed on `_id`. For a benchmark, see
<https://github.com/ropensci/nodbi#benchmark>.

## Examples

``` r
if (FALSE) { # \dontrun{
con <- src_sqlite()
print(con)
} # }
```
