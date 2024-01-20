## Test environments

* Local: macOS Darwin 21.6.0, R version 4.2.3 (2023-03-15); with CouchDB, OpenSearch, MongoDB, SQLite, PostgreSQL, DuckDB (ok)

* macOS builder: Build system: r-release-macosx-arm64|4.3.0|macosx|macOS 13.3.1 (22E261)|Mac mini|Apple M1||en_US.UTF-8|macOS 11.3|clang-1403.0.22.14.1|GNU Fortran (GCC) 12.2.0  (ok)

* Win-builder: R Under development (unstable) (2024-01-17 r85813 ucrt) using platform: x86_64-w64-mingw32 (ok)

* Github Actions: R version 4.3.2 (2023-10-31) using platform: x86_64-apple-darwin20 (64-bit); R version 4.3.2 (2023-10-31 ucrt) using platform: x86_64-w64-mingw32 (64-bit); R Under development (unstable) (2024-01-17 r85813) using platform: x86_64-pc-linux-gnu; R version 4.3.2 (2023-10-31) using platform: x86_64-pc-linux-gnu (64-bit); R version 4.2.3 (2023-03-15) using platform: x86_64-pc-linux-gnu (64-bit) (ok)

* rhub builder: Windows Server 2022, R-devel, 64 bit; Ubuntu Linux 20.04.1 LTS, R-release, GCC (ok)


## R CMD check results

0 errors | 0 warnings | 0 note


## Submission reason

* reimplemented docdb_query(), simpler, faster; return values harmonised across backends, queries can be complex, fields can be any dot path
* added functionality to retrieve all fields of all documents in collection, for any backend
* update to use faster JSONB as available from RSQLite 2.3.5 onwards
* empty parameter `query` now triggers a warning, should be a valid JSON string
* made Elasticsearch to immediately refresh index after docdb_*() operations
* made `docdb_update()` to reports which records failed to update and then continue
* refactored parts of `docdb_create()` to simplify and accelerate
* expanded testing


## revdepcheck results

We checked 2 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

* r-lib/revdepcheck reported 1 problem, with R package ctrdata; this is because one of the upstream clinical trial registers had a change in data structure that leads to an error with the released ctrdata package. I am the maintainer of ctrdata and have an updated package ready that that I will submit here as soon as this nodbi submission is accepted. 


--------

Thank you,
Ralf
