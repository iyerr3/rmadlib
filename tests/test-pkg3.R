install.packages("rmadlib_0.0.1.tar.gz", repos = NULL, type = "source")
library(rmadlib)
db.connect(host="localhost", user="qianh1", dbname="qianh1", port=5433)

x <- db.data.frame("madlibtestdata.lin_redundantobservations_oi")

dim(x)

names(x)

rmadlib:::.db.getQuery("create view v4 as select 1+23")

rmadlib:::.db.existsTable(c("public1", "v4"))

rmadlib:::.is.table.or.view(c("public", "v4"))
