## Test environments

* Local: macOS Darwin 21.6.0, R version 4.2.3 (2023-03-15); with CouchDB, OpenSearch, MongoDB, SQLite, PostgreSQL, DuckDB (ok)

* macOS builder: r-release-macosx-arm64|4.3.0|macosx|macOS 13.3.1 (22E261)|Mac mini|Apple M1||en_US.UTF-8|macOS 11.3|clang-1403.0.22.14.1|GNU Fortran (GCC) 12.2.0 (ok)

* Win-builder: R Under development (unstable) (2023-08-05 r84874 ucrt) (ok)

* Github Actions: ubuntu-20.04 R version 4.3.1; ubuntu-20.04 R Under development (unstable) (ok)


## R CMD check results

0 errors | 0 warnings | 0 note


## Submission reason

### Bug fixes

* removed explicit UTF-8 encoding reference
* corrected marginal case in `docdb_query.src_duckdb()`
* corrected minimum R version
* replaced in tests `httpbin` with `webfakes`


## revdepcheck results

We checked 2 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages


--------

Thank you,
Ralf
