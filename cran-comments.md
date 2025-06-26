## Test environments

* Local: R 4.5.1 RC (2025-06-05 r88281) on aarch64-apple-darwin20; CouchDB, OpenSearch, MongoDB, SQLite, PostgreSQL, DuckDB (all ok)

* Win-builder: R Under development (unstable) (2025-06-25 r88360 ucrt); R version 4.5.0 (2025-04-11 ucrt); R version 4.4.3 (2025-02-28 ucrt) (all ok)

* macOS builder: r-release-macosx-arm64|4.4.2|macosx|macOS 13.3.1 (22E261)|Mac mini|Apple M1||en_US.UTF-8|macOS 11.3|clang-1403.0.22.14.1|GNU Fortran (GCC) 14.2.0; r-devel-macosx-arm64|4.5.0|macosx|macOS 13.3.1 (22E261)|Mac mini|Apple M1||en_US.UTF-8|macOS 11.3|clang-1403.0.22.14.1|GNU Fortran (GCC) 14.2.0 (all ok)

* GitHub actions: windows-latest, r: 'release'; macOS-latest, r: 'release'; ubuntu-latest, r: 'oldrel-1'; ubuntu-latest, r: 'devel', including for development versions of DuckDB and RSQLite (all ok)

* Reverse dependencies: rhub::rhub_check(platforms = c("c23", "atlas")) (all ok)


## R CMD check results

0 errors | 0 warnings | 0 note


## Submission reason

Bugfixes: for DuckDB, correct mangling version number and regexp option


## Reverse dependency check results

No issues with reverse dependencies.  

--------

Thank you,
Ralf
