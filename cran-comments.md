## Test environments

* Local: macOS, R 4.1.1;  with CouchDB, ElasticSearch, MongoDB and SQLite databases
* Github Actions: Ubuntu 16.04; R version 4.1.0 (2021-05-18); R Under development (unstable) (2021-07-20 r80647); R version 4.0.5 (2021-03-31)
* win-builder: R Under development (unstable) (2021-07-21 r80649)
* R-hub builder: Windows Server 2008 R2 SP1, R-devel, 32/64 bit; Ubuntu Linux 20.04.1 LTS, R-release, GCC; Fedora Linux, R-devel, clang, gfortran

## R CMD check results

0 errors | 0 warnings | 0 notes

## Changes

- created identical API and identical return values for `docdb_*()` functions across database backends
- re-factored recently added functions for RSQLite
- performance (timing, memory) profiled and optimised

## Reverse dependencies

* I have run R CMD check on the 2 downstream dependencies
(<https://github.com/ropensci/nodbi/blob/master/revdep/README.md>),
no errors or warnings were found on reverse dependency check.

--------

Thank you -
Ralf Herold
