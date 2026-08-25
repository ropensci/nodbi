## Test environments

* Local: macOS; R Under development (unstable) (2026-06-24 r90190); CouchDB, OpenSearch, MongoDB, SQLite, PostgreSQL, DuckDB, MariaDB (ok)

* Win-builder: R version 4.5.3 (2026-03-11 ucrt); R version 4.6.1 (2026-06-24 ucrt); R Under development (unstable) (2026-08-24 r90445 ucrt)
(ok)

* macOS builder: not working at this moment

* GitHub actions: various combinations of database backends on windows-latest, r: 'release'; macOS-latest, r: 'release'; ubuntu-latest, r: 'oldrel-1'; ubuntu-latest, r: 'release'; ubuntu-latest, r: 'devel' (ok)

* Reverse dependencies: rhub::rhub_check(platforms = "atlas") (ok)


## R CMD check results

0 errors | 0 warnings | 0 note


## Submission reason

* Added support for MariaDB
* Added user option `jsonlite.pagesize` for `jsonlite::stream_{in,out}()`
* Increased default value of `pagesize` for `jsonlite::stream_{in,out}()`
* Documentation update


## Reverse dependency check results

No issues with 2 reverse dependencies.  


--------

Thank you,
Ralf
