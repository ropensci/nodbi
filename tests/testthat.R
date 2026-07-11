library(testthat)
options(duckdb.extension_directory = "~/.duckdb_extensions")
test_check("nodbi")
