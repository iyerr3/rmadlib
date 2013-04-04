install.packages("rmadlib_0.0.1.tar.gz", repos = NULL, type = "source")
library(rmadlib)
db.connect(host="localhost", user="qianh1", dbname="qianh1", port=5433)

db.disconnect(2)

db.list()

dbname(3)

user("a")

host()

conn.pkg(1)

x <- db.data.frame("madlibtestdata.lin_redundantobservations_oi")

dim(x)

names(x)

rmadlib:::.db.listTables()

rmadlib:::.is.table.or.view(c("public", "cvtest1"))

rst <- rmadlib:::.db.getQuery("select count(*) from cvtest1")

rst

rst <- rmadlib:::.db.sendQuery("select count(*) from cvtest1")

rmadlib:::.db.fetch(rst)

rst <- rmadlib:::.db.getQuery("select column_name, data_type from information_schema.columns where table_name = 'cvtest'")

rst

rmadlib:::.db.existsTable("cvtest1")

rmadlib:::.db.existsTable("tv")

rmadlib:::.db.existsTable(c("madlibtestdata", "lin_redundantobservations_oi"))

rmadlib:::.db.listFields(c("madlibtestdata", "lin_redundantobservations_oi"))

rmadlib:::.db.existsTable("tt_1")

rmadlib:::.db.listFields("tt_1")

res <- rmadlib:::.db.readTable("tt5")

str <- res$val[1]

rmadlib:::.db.str2vec(str)

x <- data.frame(a = 1:3, b = 2:4)

rmadlib:::.db.removeTable("testx1")

rmadlib:::.db.writeTable(table = "testx1", r.obj = x, distributed.by = NULL, row.names = FALSE)

res <- rmadlib:::.db.readTable("testx1")

## ------------------------------------------------------------------------

num <- as.numeric(strsplit(gsub("\\{(.*)\\}", "\\1", str), ",")[[1]])

res <- rmadlib:::.db.readTable("teststr")

str <- res$str[2]

gsub("[\\{,\\s]([^\\},]*)[,\\}\\s]", "\\1", str)

regmatches(str, gregexpr("[^,\"\\s\\{\\}]+|\"(\\\"|[^\"])*\"", str, perl=T))[[1]]

strsplit(gsub("\\{(.*)\\}", "\\1", str), ",")
