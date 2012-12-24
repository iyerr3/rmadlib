
### There is a corresponding table in database
.db.data.class <- c("data.frame.indb", "data.frame")

### There is no table in database that corresponds to this R object
### Any computation that involves uncreated will produce uncreated data type
### Thus, we need to override a number of operators
### One can only use as.data.frame.indb.data.frame.indb.uncreated
.db.data.uncreated.class <- c("data.frame.indb.uncreated", "data.frame.indb", "data.frame")
