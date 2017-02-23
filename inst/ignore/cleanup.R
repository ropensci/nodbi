# etcd
x <- etseed::etcd(host = "127.0.0.1", port = 2379, api_version = "v2",
                  allow_redirect = TRUE, scheme = "http")
keys <- vapply(x$keys()$node$nodes, "[[", "", "key")
lapply(keys, function(z) x$delete(z, recursive = TRUE))
invisible(vapply(x$keys()$node$nodes, "[[", "", "key"))

# couch
x <- sofa::Cushion$new(host = "127.0.0.1", port = 5984)
dbs <- sofa::db_list(x)
invisible(lapply(dbs, function(z) sofa::db_delete(cushion = x, dbname = z)))

# redis
con <- RedisAPI::rcppredis_hiredis(host = "127.0.0.1", port = 6379)
keys <- unlist(con$KEYS("*"))
invisible(lapply(keys, con$DEL))

# riak
x <- reeack::riak(host = "127.0.0.1", port = 8098)
keys <- x$keys(bucket = "test")$keys
invisible(lapply(keys, function(z) x$delete(key = z, bucket = 'test')))

# elasticsearch
elastic::connect()
dbs <- names(elastic::aliases_get())
invisible(lapply(dbs, elastic::index_delete))
