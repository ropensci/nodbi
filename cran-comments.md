## Test environments

* Local: macOS, R 4.1.2 and R 3.6.3; with CouchDB, Elasticsearch, MongoDB, SQLite, PostgreSQL databases
* Github Actions: Ubuntu 20.04; R release and R devel
* win-builder: R Under development (unstable) (2022-01-03 r81439 ucrt)
* R-hub builder: Debian Linux, R-devel, clang, ISO-8859-15 locale; Windows Server 2022, R-devel, 64 bit
* macOS builder: r-release-macosx-arm64|4.1.1|macosx|macOS 11.5.2 (20G95)

## R CMD check results

0 errors | 0 warnings | 0 notes

## IMPROVEMENTS
* `docdb_create` now supports file names and http urls as argument `value` for importing data
* `docdb_create` (and thus `docdb_update`) now supports quantifiers (e.g., '[a-z]{2,3}') in regular expressions

## BUG FIXES
* for SQLite, return `FALSE` like other backends when using `docdb_delete` for a non-existing container (table, in the case of SQLite)
* better handle special characters and encodings under Windows

## revdepcheck results

We checked 2 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages

--------

Thank you -
Ralf Herold
