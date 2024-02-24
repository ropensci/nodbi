## Test environments

* Local: R Under development (unstable) (2024-02-23 r85975) using platform: x86_64-apple-darwin20; with CouchDB, OpenSearch, MongoDB, SQLite, PostgreSQL, DuckDB (ok)

* macOS builder: r-release-macosx-arm64|4.3.0|macosx|macOS 13.3.1 (22E261)|Mac mini|Apple M1||en_US.UTF-8|macOS 11.3|clang-1403.0.22.14.1|GNU Fortran (GCC) 12.2.0 (ok)

* Win-builder: R Under development (unstable) (2024-02-23 r85978 ucrt) using platform: x86_64-w64-mingw32; R version 4.3.2 (2023-10-31 ucrt) using platform: x86_64-w64-mingw32 (64-bit); R version 4.2.3 (2023-03-15 ucrt) using platform: x86_64-w64-mingw32 (64-bit) (ok)

* rhub builder: Windows Server 2022, R-devel, 64 bit (ok)


## R CMD check results

0 errors | 0 warnings | 0 note


## Submission reason

* bug fix moved local variable out of UseMethod in `docdb_query`
* bug fix docTyp in `src.R`
* bug fix added missing fields validity check for DuckDB
* bug fix ensured `NULL` for all MongoDB returns

* added a first vignette
* added tests of internal functions
* more robust parameter checks in `docdb_query` and `docdb_update`


## revdepcheck results

We checked 2 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages


--------

Thank you,
Ralf
