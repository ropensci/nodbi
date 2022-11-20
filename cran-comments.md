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

 * corrected closing connections to SQL database backends upon session restart
 * improved provisions for parallel write access and corresponding tests
 * capture marginal case of no rows in docdb_query()

## revdepcheck results

Manuall checked 2 reverse dependencies, no issues.

--------

Thank you,
Ralf
