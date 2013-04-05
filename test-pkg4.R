install.packages("rmadlib_0.0.1.tar.gz", repos = NULL, type = "source")
library(rmadlib)
db.connect(host="localhost", user="qianh1", dbname="qianh1", port=5433)

x <- data.frame(a = 1:3, b = 2:4)

rmadlib:::.db.removeTable("testx1")

rmadlib:::.db.writeTable(table = "testx1", r.obj = x, distributed.by = NULL, row.names = TRUE, is.temp = TRUE)

res <- rmadlib:::.db.readTable("testx1")

res

y <- db.data.frame("testx1")

dim(y)

y@.col.data_type

y@.table.type

z <- as.db.data.frame(x, "testx8", add.row.names = TRUE, id.col = character(0), is.temp = FALSE)

z@.col.data_type

z@.table.type

w <- as.db.data.frame(x = "/Users/qianh1/workspace/rwrapper/input_data.csv", table.name = "testx8", conn.id = 1, add.row.names = FALSE, id.col = character(0), is.temp = FALSE, header = FALSE, skip=2, col.names = c(paste("x", 1:8, sep=""), "y"))

w@.col.data_type

w@.table.type


