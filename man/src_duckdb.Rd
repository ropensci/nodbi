% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/src_duckdb.R
\name{src_duckdb}
\alias{src_duckdb}
\title{Setup a DuckDB database connection}
\usage{
src_duckdb(drv = duckdb::duckdb(), dbdir = attr(drv, "dbdir"), ...)
}
\arguments{
\item{drv}{Object returned by \code{duckdb()}}

\item{dbdir}{Location for database files. Should be a path to an existing
directory in the file system. With the default (or \code{""}), all
data is kept in RAM.}

\item{...}{Additional named parameters passed on to \code{\link[DBI:dbConnect]{DBI::dbConnect()}}}
}
\value{
A \code{nodbi} source object
}
\description{
Setup a DuckDB database connection
}
\details{
Uses \code{\link[duckdb:duckdb]{duckdb::duckdb()}} as backend. \pkg{nodbi} creates or
uses a DuckDB table, with columns \verb{_id} and \code{json} created and used
by package \code{nodbi}, applying SQL functions as per
\url{https://duckdb.org/docs/extensions/json} to the \code{json} column.
Each row in the table represents a \code{JSON} document.
Any root-level \verb{_id} is extracted from the document(s) and used for
column \verb{_id}, otherwise a UUID is created as \verb{_id}.
The table is indexed on \verb{_id}.
For a benchmark, see \url{https://github.com/ropensci/nodbi#benchmark}.
}
\examples{
\dontrun{
con <- src_duckdb()
print(con)
}
}
