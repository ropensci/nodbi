## Test environments

* Local: macOS, R 4.1.2 and R 3.6.3;  with CouchDB, Elasticsearch, MongoDB SQLite and PostgreSQL databases
* Github Actions: Ubuntu 20.04; R release and R devel
* win-builder: R Under development (unstable) (2021-11-19 r81213)
* R-hub builder: Windows Server 2008 R2 SP1, R-devel, 32/64 bit; Ubuntu Linux 20.04.1 LTS, R-release, GCC; Fedora Linux, R-devel, clang, gfortran
* macOS builder

## R CMD check results

0 errors | 0 warnings | 0 notes

## Improvements

- added PostgreSQL as database backend

## Bug fix

- for SQLite under Windows add closing file references also on exit

## Reverse dependencies

* I have run R CMD check on 2 downstream dependencies, no errors or warnings
* 1 of the downstream dependencies is my package ctrdata, for which a new release will be submitted immediately

--------

Thank you -
Ralf Herold
