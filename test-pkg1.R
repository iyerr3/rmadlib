
install.packages("rmadlib_0.0.1.tar.gz", repos = NULL)

library(rmadlib)

db.connect(host="localhost", user="qianh1", dbname="qianh1", port=5433)

db.disconnect(2)

db.list()

dbname(2)

user("a")

host()

conn.pkg(1)

rmadlib:::.db.listTables()

rst <- rmadlib:::.db.getQuery("select count(*) from cvtest1")
