## Test environments

* Local: macOS 21.6.0, R 4.2.3; with CouchDB, OpenSearch, MongoDB, SQLite, PostgreSQL, DuckDB databases (ok)
* macOS builder: r-prerelease-macosx-arm64|4.3.0|macosx|macOS 13.3.1 (22E261)|Mac mini|Apple M1||en_US.UTF-8|macOS 11.3|clang-1403.0.22.14.1|GNU Fortran (GCC) 12.2.0 (ok)
* Win-builder: x86_64-w64-mingw32 (64-bit) R Under development (unstable) (2023-04-22 r84300 ucrt) (ok)
* Github Actions: ubuntu-20.04 20230409.1 R version 4.2.3 (2023-03-15) | ubuntu-20.04 20230409.1 R Under development (unstable) (2023-04-13 r84257) (ok)

* R-hub: (not working at the moment)

## R CMD check results

0 errors | 0 warnings | 1 note

Availability using Additional_repositories specification:
  ?   ?   https://duckdb.r-universe.dev

This additional repository is currently needed for nodbi under MS Windows. I raised an issue (https://github.com/duckdb/duckdb/issues/5956) because duckdb's json extension works only if duckdb is installed from this additional repository. 

## Submission reason

### Changes

* docdb_update() now can do bulk updates when _id's are in 'value' (for SQLite, DuckDB, PostgreSQL, MongoDB; not yet for CouchDB and Elastic)

### BUG FIXES

* fix tests for value parameter to be a file or an url

## revdepcheck results

We checked 2 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages

--------

Thank you,
Ralf
