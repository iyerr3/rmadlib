
install.packages(pkgs = "rmadlib_0.0.1.tar.gz", repos = NULL)

library(rmadlib)

db.connect(host="localhost", user="qianh1", dbname="qianh1")

db.listTables()

db.listColumnNames("test2")

## x <- new("indb.created")
## y <- new("indb.uncreated")

db.getQuery("create table test3(zid integer, val double precision[])")

test <- data.frame.indb("test1")

dim(test)

## v <- db.getQuery(1, paste("select count(*) from", 'test'))

v <- db.getQuery(query = paste("select column_name, data_type, numeric_precision from information_schema.columns where table_name = 'test2'", sep = ""))

w <- db.getQuery("select * from information_schema.columns where table_name = 'test3'")

db.getQuery("'\\\\d test3'")

db.getQuery("SELECT a.attnum AS ordinal_position,
a.attname AS column_name,
t.typname AS data_type,
a.attlen AS character_maximum_length,
a.atttypmod AS modifier,
a.attnotnull AS notnull,
a.atthasdef AS hasdefault
FROM pg_class c,
pg_attribute a,
pg_type t
WHERE c.relname = 'test3'
AND a.attnum > 0
AND a.attrelid = c.oid
AND a.atttypid = t.oid
ORDER BY a.attnum")
