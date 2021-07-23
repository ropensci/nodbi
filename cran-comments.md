## Test environments

* Local: macOS, R 4.1.0; R 3.6.3 with CouchDB, redis, MongoDB and SQLite databases
* Github Actions: Ubuntu 16.04; R version 4.1.0 (2021-05-18); R Under development (unstable) (2021-07-20 r80647); R version 4.0.5 (2021-03-31)
* win-builder: R Under development (unstable) (2021-07-21 r80649)
* R-hub builder: Windows Server 2008 R2 SP1, R-devel, 32/64 bit; Ubuntu Linux 20.04.1 LTS, R-release, GCC; Fedora Linux, R-devel, clang, gfortran

## R CMD check results

0 errors | 0 warnings | 0 notes

## Changes

- implement handling of JSON elements that are objects with 
  src_sqlite (in addition to arrays, strings, numbers) for 
  docdb_query, in analogy to other src_* databases
- simplified docdb_exists()
- docdb_update() for src_mongo() now returns the number of
  upserted or matched documents, irrespective of whether
  they were updated or not
- require RSQLite 2.2.4 released on 2021-03-12 in order to use
  transactions with docdb_{create,update,query,delete}.src_sqlite()

## Maintainer change

The previous maintainer, Scott Chamberlain (sckott@protonmail.com)
transferred the package to me for further maintenance. 
Please see Scott's email to CRAN@R-project.org of 2021-07-19.

## Reverse dependencies

* I have run R CMD check on the 2 downstream dependencies
(<https://github.com/ropensci/nodbi/blob/master/revdep/README.md>),
no errors or warnings were found on reverse dependency check.

--------

Thank you -
Ralf Herold
