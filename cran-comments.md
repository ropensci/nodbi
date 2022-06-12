## Test environments

* Local: macOS, R 4.1.2 and R 3.6.3; with CouchDB, OpenSearch, MongoDB, SQLite, PostgreSQL databases
* Github Actions: Ubuntu 20.04; R release and R devel
* win-builder: R Under development (unstable) (2022-06-09 r82474 ucrt)
* R-hub builder: 	Windows Server 2022, R-devel, 64 bit; Ubuntu Linux 20.04.1 LTS, R-release, GCC; Fedora Linux, R-devel, clang, gfortran
* macOS builder: r-devel-macosx-arm64|4.2.0|macosx|macOS 11.5.2 (20G95)|Mac mini|Apple M1||en_US.UTF-8

## R CMD check results

0 errors | 0 warnings | 0 notes

## Submission reason

### Changes
* refactored `docdb_update.src_couchdb` to use `jqr`
* adapted `docdb_create` to accept `jsonlite`, `jsonify`, `jqr` JSON
* added details to README
* testing (unset LANG, relocate open code, better cleaning up)

## revdepcheck results

We checked 2 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages

--------

Thank you,
Ralf
