
require(RPostgreSQL)

drv <- dbDriver("PostgreSQL")

conn <- dbConnect(drv = drv, host = "localhost", dbname = "qianh1", user = "qianh1", password = "")

dbListConnections(drv)

dbListTables(conn) #

rs <- dbSendQuery(conn, "select count(*) from madlibtestdata.lin_winequality_white_oi") #

dbGetRowCount(rs)

dbColumnInfo(rs)

dbGetStatement(rs)

dbGetInfo(rs)

dbHasCompleted(rs)

data <- fetch(rs, n = -1)

dbGetQuery(conn, "select count(*) from madlibtestdata.lin_winequality_white_oi") #

dbListFields(conn, "elastic_net_output") #

dbReadTable(conn, "elastic_net_output")

dbReadTable(conn, "test")

x <- data.frame(a = 1:4, b = 2:5)

## need distributed by
dbWriteTable(conn, name = "testx", value = x) #

y <- data.frame(a = 1:4, b = 2:5, row.names = 3:6)

dbWriteTable(conn, name = "testy", value = y)

dbRemoveTable(conn, "testy")

z <- dbReadTable(conn, "testx", row.names = a)

z <- dbReadTable(conn, "testx", row.names = "2")
