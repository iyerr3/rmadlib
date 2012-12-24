
### There is a corresponding table in database
.db.data.class <- c("data.frame.indb", "data.frame")

### There is no table in database that corresponds to this R object
### Any computation that involves unsynced will produce unsynced data type
### Thus, we need to override a number of operators
.db.data.unsynced.class <- c("data.frame.unsynced", "data.frame")
