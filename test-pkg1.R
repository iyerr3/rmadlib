
install.packages("rmadlib_0.0.1.tar.gz", repos = NULL)

library(rmadlib)

db.connect(host="localhost", user="qianh1", dbname="qianh1", port=5433)

db.list()

dbname()

user()

host()

conn.pkg(2)
