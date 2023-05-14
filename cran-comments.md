## Test environments

* Local: macOS Monterey 12.6.5, R version 4.2.3 (2023-03-15); with CouchDB, OpenSearch, MongoDB, SQLite, PostgreSQL, DuckDB databases (ok)
* macOS builder: r-prerelease-macosx-arm64|4.3.0|macosx|macOS 13.3.1 (22E261)|Mac mini|Apple M1||en_US.UTF-8|macOS 11.3|clang-1403.0.22.14.1|GNU Fortran (GCC) 12.2.0  (ok)
* Win-builder: x86_64-w64-mingw32, R Under development (unstable) (2023-05-13 r84429 ucrt) (ok)
* Github Actions: ubuntu-20.04 20230409.1 R version 4.2.3 (2023-03-15) | ubuntu-20.04 20230409.1 R Under development (unstable) (2023-04-13 r84257) (ok)
* R-hub: Ubuntu Linux 20.04.1 LTS, R-release, GCC; Windows Server 2022, R-devel, 64 bit; Fedora Linux, R-devel, clang, gfortran (OK)

## R CMD check results

0 errors | 0 warnings | 0 note

## Submission reason

### BUG FIXES

* fix initialisation in docdb_query with src_duckdb

## revdepcheck results

We checked 2 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages

--------

Thank you,
Ralf
