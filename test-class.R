
x <- 1
class(x) <- c("db.obj", "db.data.frame", "data.frame")

setClass("db.obj",
         representation(
             .name = "character", # object name
             .con.id = "numeric", # connection ID
             ## used for identify connection
             .dbname = "character", # database name
             .host = "character", # database server
             .con.pkg = "character", # R package used for connection
             ## table properties
             .col.names = "character", # column names
             .col.types = "character" # column types
             )
         )

y <- new("db.obj")

attributes(y)
attr(y, "a") <- 1

z <- 1
attr(z, "a") <- 10
