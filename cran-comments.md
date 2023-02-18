## Test environments

* Local: macOS 21.6.0, R 4.3.0; with CouchDB, OpenSearch, MongoDB, SQLite, PostgreSQL, DuckDB databases
* Github Actions: Ubuntu 20.04; R release and R devel
* win-builder: R Under development (unstable) (2022-10-11 r83083 ucrt)
* R-hub: Windows Server 2022, R-devel, 64 bit
* macOS builder: r-release-macosx-arm64|4.2.1|macosx|macOS 11.5.2 (20G95)|Mac mini|Apple M1||en_US.UTF-8

## R CMD check results

0 errors | 0 warnings | 1 note

Availability using Additional_repositories specification:
  ?   ?   https://duckdb.r-universe.dev

## Submission reason

### Bug fixes

* src_duckdb(): handle when json_type returns NULL for non-existing path (2023-02-18)

### Changes

* added warning if DuckDB's JSON extension is not available; see also issue #45
* minor simplification of docdb_exists() for src_mongo()

## revdepcheck results

Manuall checked 2 reverse dependencies, no issues.

--------

Thank you,
Ralf
