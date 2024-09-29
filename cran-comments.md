## Test environments

* Local: R version 4.4.1 (2024-06-14) using platform: x86_64-apple-darwin20; with CouchDB, OpenSearch, MongoDB, SQLite, PostgreSQL, DuckDB (ok)

* Win-builder: R version 4.4.1 (2024-06-14 ucrt); R version 4.3.3 (2024-02-29 ucrt); R Under development (unstable) (2024-09-28 r87201 ucrt) (ok)

* macOS builder: r-devel-macosx-arm64|4.4.0|macosx|macOS 13.3.1 (22E261)|Mac mini|Apple M1||en_US.UTF-8|macOS 11.3|clang-1403.0.22.14.1|GNU Fortran (GCC) 12.2.0 (ok)

* GitHub actions: windows-2022, r: 'release'; macOS-latest, r: 'release'; ubuntu-latest, r: 'oldrel-1'; ubuntu-latest, r: 'devel' (ok)


## R CMD check results

0 errors | 0 warnings | 0 note


## Submission reason

* update to new `duckdb` release 1.11.0
* accelerate creating and updating from file


## Reverse dependency check results

I checked two reverse dependencies locally and found no problems. 

--------

Thank you,
Ralf
