
install.packages(pkgs = "rmadlib_0.0.1.tar.gz", repos = NULL)

library(rmadlib)

db.connect(host="localhost", user="qianh1", dbname="qianh1")

db.listTables()

db.listColumnNames("test")

## x <- new("indb.created")
## y <- new("indb.uncreated")

test <- data.frame.indb("test")

dim(test)

## v <- db.getQuery(1, paste("select count(*) from", 'test'))

v <- db.getQuery(query = paste("select column_name from information_schema.columns where table_name = '", test@.table, "'", sep = ""))
