## Test environments

* Local: R version 4.4.2 (2024-10-31) using platform: aarch64-apple-darwin20; with CouchDB, OpenSearch, MongoDB, SQLite, PostgreSQL, DuckDB (ok)

* Win-builder: R version 4.4.2 (2024-10-31 ucrt); R version 4.3.3 (2024-02-29 ucrt) (ok)

* macOS builder: r-devel-macosx-arm64|4.4.0|macosx|macOS 13.3.1 (22E261)|Mac mini|Apple M1||en_US.UTF-8|macOS 11.3|clang-1403.0.22.14.1|GNU Fortran (GCC) 12.2.0 (ok)

* GitHub actions: windows-latest, r: 'release'; macOS-latest, r: 'release'; ubuntu-latest, r: 'oldrel-1'; ubuntu-latest, r: 'devel' (ok)


## R CMD check results

0 errors | 0 warnings | 0 note


## Submission reason

* more consistent types in columns of returned data frames
* create and update from file, depending on RSQLite version
* disable internet resource in vignette (CRAN comment)
* explicit database version checks, replacing version in Suggests
* refactoring, improving code and user info


## Reverse dependency check results

I checked two reverse dependencies using rhub::rhub_check(platforms = "atlas") and found no problems. 

--------

Thank you,
Ralf
