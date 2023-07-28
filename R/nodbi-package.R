#' Document database connector
#'
#' Simplified document database access and manipulation,
#' providing a common API across supported 'NoSQL' databases
#' 'Elasticsearch', 'CouchDB', 'MongoDB' as well as
#' 'SQLite/JSON1', 'PostgreSQL' and 'DuckDB'.
#'
#' @name nodbi-package
#' @docType package
#' @author Scott Chamberlain \email{sckott@@protonmail.com}
#' @author Rich FitzJohn \email{rich.fitzjohn@@gmail.com}
#' @author Jeroen Ooms \email{jeroen.ooms@@stat.ucla.edu}
#' @author Ralf Herold \email{ralf.herold@@mailbox.org}
#' @keywords package
NULL

#' This function is defunct.
#' @rdname src_etcd-defunct
#' @keywords internal
#' @noRd
src_etcd <- function() {
  .Defunct(msg = "This function is defunct; etcd removed")
}

#' This function is defunct.
#' @rdname src_redis-defunct
#' @keywords internal
#' @noRd
src_redis <- function() {
  .Defunct(msg = "This function is defunct; redis removed")
}

#' Defunct functions in nodbi
#'
#' - [src_etcd]: etcd removed, with all its S3 methods for `docdb_*`
#' - [src_redis]: redis removed, with all its S3 methods for `docdb_*`
#'
#' @name nodbi-defunct
#' @noRd
NULL

#' Data set 'diamonds'
#'
#' @format A data frame with 53940 rows and 10 variables:
#'
#' - price price in US dollars (326-18,823 USD)
#' - carat weight of the diamond (0.2-5.01)
#' - cut quality of the cut (Fair, Good, Very Good, Premium, Ideal)
#' - color diamond colour, from J (worst) to D (best)
#' - clarity a measurement of how clear the diamond is (I1 (worst),
#'   SI1, SI2, VS1, VS2, VVS1, VVS2, IF (best))
#' - x length in mm (0-10.74)
#' - y width in mm (0-58.9)
#' - z depth in mm (0-31.8)
#' - depth total depth percentage = z / mean(x, y) = 2 * z / (x + y)
#'   (43-79)
#' - table width of top of diamond relative to widest point (43-95)
#'
#' @source from \pkg{ggplot2}
#' @name diamonds
#' @docType data
#' @keywords datasets
NULL


#' Data set 'contacts'
#'
#' @format A JSON string with ragged, nested contact details
#' @name contacts
#' @docType data
#' @keywords datasets
#' @export
contacts <- '
[
  {
    "_id": "5cd67853f841025e65ce0ce2",
    "isActive": false,
    "balance": "$3,808.45",
    "age": 23,
    "eyeColor": "green",
    "name": "Lacy Chen",
    "email": "lacychen@conjurica.com",
    "about": "Sunt consequat ad dolore. Exercitation nisi reprehenderit.",
    "registered": "2014-08-03T12:11:54 -02:00",
    "tags": [
      "nulla",
      "nisi",
      "adipisicing",
      "do",
      "ad",
      "ullamco",
      "irure"
    ],
    "friends": [
      {
        "id": 0,
        "name": "Wooten Goodwin"
      },
      {
        "id": 1,
        "name": "Brandie Woodward"
      },
      {
        "id": 2,
        "name": "Angelique Britt"
      }
    ]
  },
  {
    "_id": "5cd678531b423d5f04cfb0a1",
    "isActive": false,
    "balance": "$3,400.50",
    "age": 20,
    "eyeColor": "brown",
    "name": "Rae Colon",
    "email": "raecolon@conjurica.com",
    "about": "Nisi excepteur duis duis aliquip qui id consequat consequat.",
    "registered": "2018-12-19T06:23:35 -01:00",
    "tags": [
      "nostrud",
      "eu",
      "consectetur",
      "adipisicing",
      "labore",
      "ut",
      "voluptate"
    ],
    "friends": [
      {
        "id": 0,
        "name": "Yang Yates"
      },
      {
        "id": 1,
        "name": "Lacy Chen"
      }
    ]
  },
  {
    "_id": "5cd6785335b63cb19dfa8347",
    "isActive": false,
    "balance": "$2,579.09",
    "age": 30,
    "eyeColor": "brown",
    "name": "Williamson French",
    "email": "williamsonfrench@conjurica.com",
    "about": "Nulla do sunt consectetur officia. Laboris pariatur incididunt.",
    "registered": "2018-02-14T10:59:57 -01:00",
    "tags": [
      "exercitation",
      "do",
      "magna",
      "ut",
      "consectetur",
      "ex",
      "incididunt"
    ],
    "friends": [
      {
        "id": 0,
        "name": "Coleen Dunn"
      },
      {
        "id": 1,
        "name": "Doris Phillips"
      },
      {
        "id": 2,
        "name": "Concetta Turner"
      }
    ]
  },
  {
    "_id": "5cd6785325ce3a94dfc54096",
    "isActive": true,
    "balance": "$1,161.52",
    "age": 22,
    "eyeColor": "brown",
    "name": "Pace Bell",
    "email": "pacebell@conjurica.com",
    "about": "Eiusmod sunt laborum ipsum do cupidatat qui id dolore do.",
    "registered": "2018-08-17T12:23:42 -02:00",
    "tags": [
      "aliqua",
      "consectetur",
      "commodo",
      "velit",
      "cupidatat",
      "duis",
      "dolore"
    ],
    "friends": [
      {
        "id": 0,
        "name": "Baird Keller"
      },
      {
        "id": 1,
        "name": "Francesca Reese"
      },
      {
        "id": 2,
        "name": "Dona Bartlett"
      }
    ]
  },
  {
    "_id": "5cd678530df22d3625ed8375",
    "isActive": true,
    "balance": "$2,412.67",
    "age": 20,
    "eyeColor": "blue",
    "name": "Krista Baxter",
    "email": "kristabaxter@conjurica.com",
    "about": "Sint quis nulla ea fugiat. Commodo nisi qui eu sit.",
    "registered": "2017-07-19T05:03:47 -02:00",
    "tags": [
      "sit",
      "cillum",
      "commodo",
      "labore",
      "sint",
      "in",
      "exercitation"
    ],
    "friends": [
      {
        "id": 0,
        "name": "Pace Bell"
      }
    ]
  }
]
'


#' Data set 'mapdata'
#'
#' @format A JSON string with ragged, nested travel details
#' @name mapdata
#' @docType data
#' @keywords datasets
#' @export
mapdata <- '
  [{"destination_addresses": [
    "Miami, FL, USA",
    "Austin, TX, USA",
    "Napa County, CA, USA"
  ],
  "origin_addresses": [
    "Santa Barbara, CA, USA",
    "New York, NY, USA"
  ],
  "rows": [{
    "elements": [{
        "distance": {
          "text": "227 mi",
          "somevalue": 365468
        },
        "duration": {
          "text": "3 hours 54 mins",
          "somevalue": 14064
        },
        "status": "OK"
      },
      {
        "distance": {
          "text": "2,878 mi",
          "somevalue": 4632197
        },
        "duration": {
          "text": "1 day 18 hours",
          "somevalue": 151772
        },
        "status": "OK"
      },
      {
        "distance": {
          "text": "1,286 mi",
          "somevalue": 2069031
        },
        "duration": {
          "text": "18 hours 43 mins",
          "somevalue": 67405
        },
        "status": "OK"
      },
      {
        "distance": {
          "text": "2,871 mi",
          "somevalue": 4620514
        },
        "duration": {
          "text": "1 day 18 hours",
          "somevalue": 152913
        },
        "status": "OK"
      }
    ]
  }],
  "status": "OK"},

  {"destination_addresses": [
    "Washington, DC, USA",
    "Philadelphia, PA, USA",
    "Napa County, CA, USA"
  ],
  "origin_addresses": [
    "New York, NY, USA"
  ],
  "rows": [{
    "elements": [{
        "distance": {
          "text": "227 mi",
          "somevalue": 365468
        },
        "duration": {
          "text": "3 hours 54 mins",
          "somevalue": 14064
        },
        "status": "OK"
      },
      {
        "distance": {
          "text": "2,878 mi",
          "somevalue": 4632197
        },
        "duration": {
          "text": "1 day 18 hours",
          "somevalue": 151772
        },
        "status": "OK"
      },
      {
        "distance": {
          "text": "1,286 mi",
          "somevalue": 2069031
        },
        "duration": {
          "text": "18 hours 43 mins",
          "somevalue": 67405
        },
        "status": "OK"
      },
      {
        "distance": {
          "text": "1,742 mi",
          "somevalue": 2802972
        },
        "duration": {
          "text": "1 day 2 hours",
          "somevalue": 93070
        },
        "status": "OK"
      },
      {
        "distance": {
          "text": "2,871 mi",
          "somevalue": 4620514
        },
        "duration": {
          "text": "1 day 18 hours",
          "somevalue": 152913
        },
        "status": "OK"
      }
    ]
  }],
  "status": "OK"}
  ]
'
