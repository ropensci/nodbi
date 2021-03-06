% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/query.R
\name{docdb_query}
\alias{docdb_query}
\title{Get documents with a filtering query}
\usage{
docdb_query(src, key, query, ...)
}
\arguments{
\item{src}{source object, result of call to src}

\item{key}{(character) A key (collection for mongo)}

\item{query}{various, see Query section below.}

\item{...}{Additional named parameters passed on to each package:
\itemize{
\item CouchDB: passed on to \code{\link[sofa:db_query]{sofa::db_query()}}
\item Elasticsearch: passed on to \code{\link[elastic:Search]{elastic::Search()}}
\item MongoDB: passed on to the \verb{$find} method of \pkg{mongolite}
}}
}
\description{
Get documents with a filtering query
}
\details{
Note that with etcd, you have to prefix a key with a
forward slash.
}
\section{What is expected for each source}{

\itemize{
\item CouchDB: a list, see docs for \code{\link[sofa:db_query]{sofa::db_query()}}
\item Elasticsearch: query parameters, see \code{\link[elastic:Search]{elastic::Search()}}; passed to
the \code{query} parameter of \code{elastic::Search}, thus performs a URI
based search where the query is passed in the URI instead of the body.
In theory you can instead pass in a JSON or list to the \code{body}
parameter, but if you want to do complicated Elasticsearch queries
you may be better of using \pkg{elastic} package directly
\item MongoDB: a JSON \code{query} string and optionally other parameters
such as \code{fields}, see \pkg{mongolite}
\item SQLite: arameter \code{query}, a JSON string; supported at the moment
is only one level of $or or $and operators with
$eq, $gt, $gte, $lt, $lte, $ne and $regex as tests.
Optionally, \code{fields} a JSON string of fields to be
returned from anywhere in the tree (in dot paths notation).
}
}

\section{Not supported yet}{

\itemize{
\item Redis
}
}

\examples{
\dontrun{
# CouchDB
(src <- src_couchdb())
if (docdb_exists(src, "mtcars2")) docdb_delete(src, "mtcars2")
invisible(docdb_create(src, key = "mtcars2", value = mtcars))
docdb_exists(src, "mtcars2")
(query <- list(cyl = list("$gt" = 6)))
docdb_query(src, "mtcars2", query = query)

# Elasticsearch
src <- src_elastic()
if (docdb_exists(src, "iris")) docdb_delete(src, "iris")
docdb_create(src, "iris", iris)
docdb_exists(src, "iris")
Sys.sleep(2)
docdb_query(src, "iris", query = "setosa")
docdb_query(src, "iris", query = "1.5")
docdb_query(src, "iris", query = "Petal.Width:1.5")

# Mongo
src <- src_mongo(collection = "mtcars")
if (docdb_exists(src, "mtcars")) docdb_delete(src, "mtcars")
docdb_create(src, "mtcars", mtcars)
docdb_query(src, "mtcars", query = '{"mpg":21}')
docdb_query(src, "mtcars", query = '{"mpg":21}', fields = '{"mpg":1, "cyl":1}')
docdb_get(src, "mtcars")

# SQLite
src <- src_sqlite()

# various query combinations:
docdb_create(src, "mtcars", mtcars)
docdb_query(src, "mtcars", query = '{"mpg":21}')
docdb_query(src, "mtcars", query = '{"mpg":21}', fields = '{"mpg":1, "cyl":1}')
docdb_query(src, "mtcars", query = '{"_id": {"$regex": "^.+0.*$"}}', fields = '{"gear": 1}')

# regular expressions except for [...] can be used to select fields:
docdb_create(src, "mapdata", value = data.frame(mapdata, stringsAsFactors = FALSE))
docdb_query(src, "mapdata", fields = '{"rows.elements.d.*text": 1}', query = '{}')
docdb_query(src, "mapdata", fields = '{"rows.elements.\\\\\\\\S+.text": 1}', query = '{}')
docdb_query(src, "mapdata", fields = '{"rows.elements.*somevalue": 1}', query = '{}')
}
}
