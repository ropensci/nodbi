## Test environments

* Local: R version 4.4.1 (2024-06-14) using platform: x86_64-apple-darwin20; with CouchDB, OpenSearch, MongoDB, SQLite, PostgreSQL, DuckDB (ok)

* Win-builder: R Under development (unstable) (2024-07-24 r86924 ucrt); R version 4.3.3 (2024-02-29 ucrt); R version 4.4.1 (2024-06-14 ucrt) (ok)

* GitHub actions: windows-2022, r: 'release'; macOS-latest, r: 'release'; ubuntu-latest, r: 'oldrel-1'; ubuntu-latest, r: 'devel' (ok)


## R CMD check results

0 errors | 0 warnings | 0 note


## Submission reason

- bug fix: `docdb_query()` not working for cases when dot paths had no counts between fields
- bug fix: wrong database information printed
- bug fix: correct SQL to speed up `docdb_query()`


## Reverse dependency check results

I checked two reverse dependencies locally and found no problems. 

--------

Thank you,
Ralf
