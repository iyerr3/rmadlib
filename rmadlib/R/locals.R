## Private variables of the package

## All local variables defined at the package loading time
## cannot be changed without exposing to users. If we really
## export these variables, they will easily interfere with other
## user defined variables.
## The only way is to define a local environment inside the package.
## The environment is a constant and cannot be changed,
## but the variables inside this environment can be changed.
## The environment constant is not exported, and is thus hidden
## from the users.
.localVars <- new.env()

## create class structure ----------------
## The R object has its corresponding table/view in database
setClass("indb.created",
         representation(
             .name = "character", # table name
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

## table, a sub-class of indb.created
setClass("indb.table",
         representation(
             .id.col = "character", # which column is used to identify different rows
             .dim = "numeric" # dimension of table
             ),
         contains = "indb.created")

## view, a sub-class of indb.created
setClass("indb.view",
         representaion(
             .query = "character" # the view query
             ),
         contains = "indb.created")

## The R object has no coresponding table in database
## It is generated in the middle of computations that involve
## "indb.created" or "indb.uncreated" objects.
## It can be converted to "indb.created" via as.data.frame.indb()
## More precisely, it is a view existing in R only
## It can be converted into indb.created objects
setClass("indb.uncreated",
         representation(
             .query = "character",
             .con.id = "numeric",
             .dbname = "character",
             .host = "character",
             .con.pkg = "character"
             )
         )

## Abstract interface, which is the parent of both classes
## defined in the above.
## Many functions in this package should operate on both classes.
## So we define this abstract class.
setClassUnion("data.frame.indb", members = c("indb.created", "indb.uncreated"))
