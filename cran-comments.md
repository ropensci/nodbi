## Test environments

* Local: R version 4.5.0 (2025-04-11), aarch64-apple-darwin20 macOS Sequoia 15.5, CouchDB, OpenSearch, MongoDB, SQLite, PostgreSQL, DuckDB (ok)

* Win-builder: R version 4.4.3 (2025-02-28 ucrt); R Under development (unstable) (2025-06-02 r88257 ucrt); R version 4.5.0 (2025-04-11 ucrt) (ok)

* macOS builder: r-devel-macosx-arm64|4.5.0|macosx|macOS 13.3.1 (22E261)|Mac mini|Apple M1||en_US.UTF-8|macOS 11.3|clang-1403.0.22.14.1|GNU Fortran (GCC) 14.2.0; r-release-macosx-arm64|4.4.2|macosx|macOS 13.3.1 (22E261)|Mac mini|Apple M1||en_US.UTF-8|macOS 11.3|clang-1403.0.22.14.1|GNU Fortran (GCC) 14.2.0 (ok)

* GitHub actions: windows-latest, r: 'release'; macOS-latest, r: 'release'; ubuntu-latest, r: 'oldrel-1'; ubuntu-latest, r: 'devel', including for development versions of DuckDB and RSQLite (ok)


## R CMD check results

0 errors | 0 warnings | 0 note


## Submission reason

* Adapt to `duckdb` 1.3.0 (e.g., use new `json_tree` function)
* Harmomise printing connections, warning for non-persistent connections 


## Reverse dependency check results

No issues with two reverse dependencies using rhub::rhub_check(platforms = "atlas"). 

--------

Thank you,
Ralf
