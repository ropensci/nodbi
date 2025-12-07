## Test environments

* Local: R version 4.5.1 Patched (2025-09-12 r88822) on aarch64-apple-darwin20; CouchDB, OpenSearch, MongoDB, SQLite, PostgreSQL, DuckDB (all ok)

* Win-builder: R version 4.4.3 (2025-02-28 ucrt); R version 4.5.2 (2025-10-31 ucrt); R Under development (unstable) (2025-12-05 r89107 ucrt) (all ok)

* macOS builder: currently not working

* GitHub actions: windows-latest, r: 'release'; macOS-latest, r: 'release'; ubuntu-latest, r: 'oldrel-1'; ubuntu-latest, r: 'devel' (all ok)

* Reverse dependencies: rhub::rhub_check(platforms = c("c23", "atlas")) (all ok)


## R CMD check results

0 errors | 0 warnings | 0 note


## Submission reason

* Refactoring (use `jsonb_tree` from `RSQLite` 2.4.4; avoid time-costly `json_tree` for `src_duckdb`)
* Bugfixes (fix `docdb_query` for `src_sqlite` for some `$in` queries)
* Update database version requirements, reduce version-dependent code


## Reverse dependency check results

No issues with reverse dependencies.  

--------

Thank you,
Ralf
