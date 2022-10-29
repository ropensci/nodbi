## Test environments

* Local: macOS 21.6.0, R 4.1.2; with CouchDB, OpenSearch, MongoDB, SQLite, PostgreSQL, DuckDB databases
* Github Actions: Ubuntu 20.04; R release and R devel
* win-builder: R Under development (unstable) (2022-10-11 r83083 ucrt), x86_64-w64-mingw32 (64-bit)
* R-hub: Windows Server 2022, R-devel, 64 bit; Ubuntu Linux 20.04.1 LTS, R-release, GCC; Fedora Linux, R-devel, clang, gfortran
* macOS builder: r-release-macosx-arm64|4.2.1|macosx|macOS 11.5.2 (20G95)|Mac mini|Apple M1||en_US.UTF-8

## R CMD check results

0 errors | 0 warnings | 1 note

Availability using Additional_repositories specification:
  ?   ?   https://duckdb.r-universe.dev

## Submission reason

 * resubmitted after fixing a bug in docdb_query related to the new backend storage

### Changes

 * now can also use DuckDB as new backend storage
 * DuckDB 0.6.0 or higher is needed, so far only in repository as per note
 * DESCRIPTION has in Suggests: duckdb (>= 0.6.0)

## revdepcheck results

We checked 2 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages

--------

Thank you,
Ralf
