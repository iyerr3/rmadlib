install.packages("rmadlib_0.0.1.tar.gz", repos = NULL)
library(rmadlib)
db.connect(host="localhost", user="qianh1", dbname="qianh1", port=5433)

db.disconnect(1)

db.list()

dbname(2)

user("a")

host()

conn.pkg(1)

x <- db.data.frame("madlibtestdata.lin_redundantobservations_oi")

rmadlib:::.db.listTables()

rmadlib:::.is.table.or.view("cvtest1")

rst <- rmadlib:::.db.getQuery("select count(*) from cvtest1")

rst <- rmadlib:::.db.sendQuery("select count(*) from cvtest1")

rst <- rmadlib:::.db.getQuery("select column_name, data_type from information_schema.columns where table_name = 'cvtest'")

rmadlib:::.db.fetch(rst)

rmadlib:::.db.existsTable("cvtest")

rmadlib:::.db.existsTable("tv")

rmadlib:::.db.existsTable(c("madlibtestdata", "lin_redundantobservations_oi"))

rmadlib:::.db.listFields(c("madlibtestdata", "lin_redundantobservations_oi"))

## rmadlib:::.db.listFields("madlibtestdata.lin_redundantobservations_oi")

rmadlib:::.db.existsTable("tt")

rmadlib:::.db.listFields("tt")

rmadlib:::.db.removeTable("testx1")

res <- rmadlib:::.db.readTable("tt5")

str <- res$val[1]

rmadlib:::.db.str2vec(str)

x <- data.frame(a = 1:3, b = 2:4)

rmadlib:::.db.writeTable(table = "testx1", r.obj = x, distributed.by = NULL, row.names = FALSE)

## ------------------------------------------------------------------------

num <- as.numeric(strsplit(gsub("\\{(.*)\\}", "\\1", str), ",")[[1]])

res <- rmadlib:::.db.readTable("teststr")

str <- res$str[2]

gsub("[\\{,\\s]([^\\},]*)[,\\}\\s]", "\\1", str)

regmatches(str, gregexpr("[^,\"\\s\\{\\}]+|\"(\\\"|[^\"])*\"", str, perl=T))[[1]]

strsplit(gsub("\\{(.*)\\}", "\\1", str), ",")
