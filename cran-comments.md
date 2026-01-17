## Test environments

* Local: R Under development (unstable) (2026-01-16 r89305) aarch64-apple-darwin20; CouchDB, OpenSearch, MongoDB, SQLite, PostgreSQL, DuckDB (ok)

* Win-builder:  (ok)

* macOS builder: Build system: r-devel-macosx-arm64|4.6.0|macosx|macOS 26.2 (25C56)|Mac mini|Apple M1||en_US.UTF-8|macOS 14.4|clang-1700.6.3.2|GNU Fortran (GCC) 14.2.0 (ok)

* GitHub actions: windows-latest, r: 'release'; macOS-latest, r: 'release'; ubuntu-latest, r: 'oldrel-1'; ubuntu-latest, r: 'release'; ubuntu-latest, r: 'devel' (ok)

* Reverse dependencies: rhub::rhub_check(platforms = c("c23", "atlas")) (ok)


## R CMD check results

0 errors | 0 warnings | 1 note


## Submission reason

* Package elastic in Suggests has been archived, not yet clear who / if I will take over maintaining the package
* In the meantime, indicate that elastic can be installed from Github repository 
* Note is triggered by the new field Remote in DESCRIPTION to point to the Github repository
* Other changes to address that package elastic was archived (user documentation, continuous integration)


## Reverse dependency check results

No issues with reverse dependencies.  

--------

Thank you,
Ralf
