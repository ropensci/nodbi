## Test environments

* Local: macOS Darwin 21.6.0, R version 4.2.3 (2023-03-15); with CouchDB, OpenSearch, MongoDB, SQLite, PostgreSQL, DuckDB (ok)

* macOS builder: r-devel-macosx-arm64|4.4.0|macosx|macOS 13.3.1 (22E261)|Mac mini|Apple M1||en_US.UTF-8|macOS 11.3|clang-1403.0.22.14.1|GNU Fortran (GCC) 12.2.0 (ok)

* Win-builder: R Under development (unstable) (2024-02-02 r85855 ucrt) x86_64-w64-mingw32 (ok)

* rhub builder: Windows Server 2022, R-devel, 64 bit; Ubuntu Linux 20.04.1 LTS, R-release, GCC (ok)


## R CMD check results

0 errors | 0 warnings | 0 note


## Submission reason

* bug fix following recent switch to JSONB with RSQLite: add missing back-casting to JSON in docdb_get.src_sqlite() once any docdb_update() has been applied


## revdepcheck results

We checked 2 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages

--------

Thank you,
Ralf
