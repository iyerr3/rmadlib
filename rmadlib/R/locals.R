.localVars <- new.env()

## create class structure ----------------
## The R object has its corresponding table in database
setClass("indb.created",
         representation(
             .table = "character", # table name
             .con.id = "numeric", # connection ID
             .order.by = "character", # which column is used to order rows
             .dim = "numeric", # dimension of table
             .col.names = "character" # column names
             )
         )

## The R object has no coresponding table in database
## It is generated in the middle of computations that involve
## "indb.created" or "indb.uncreated" objects.
## It can be converted to "indb.created" via as.data.frame.indb()
setClass("indb.uncreated",
         representation(
             .query = "character",
             .tables = "character",
             .con.id = "numeric"
             )
         )

setClassUnion("data.frame.indb", members = c("indb.created", "indb.uncreated"))
