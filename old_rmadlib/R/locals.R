
## ------------------------------------------------------------------------
## Private variables of the package

## All local variables defined at the package loading time
## cannot be changed without exposing to users. If we really
## export these variables, they will easily interfere with other
## user defined variables.
## 
## The only way is to define a local environment inside the package.
##
## The environment is a constant and cannot be changed, 
## but the variables inside this environment can be changed.
## The environment constant is not exported, and is thus hidden
## from the users.
## ------------------------------------------------------------------------

.localVars <- new.env()

## create class structure ----------------

## The R object has its corresponding table/view in database
## object in database
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

## table, a sub-class of db.obj
setClass("db.obj.table",
         representation(
             .id.col = "character", # which column is used to identify different rows
             .dim = "numeric" # dimension of table
             ),
         contains = "db.obj")

## view, a sub-class of db.obj
setClass("db.obj.view",
         representation(
             .query = "character" # the view query
             ),
         contains = "db.obj")

## The R object has no coresponding table in database
## It is generated in the middle of computations that involve
## "db.obj" or "query.obj" objects.
## It can be converted to "db.obj" via realize(), which returns
## a db.obj
## More precisely, it is a view existing in R only
## It can be converted into db.obj objects
setClass("query.obj",
         representation(
             .query = "character",
             .con.id = "numeric",
             .dbname = "character",
             .host = "character",
             .con.pkg = "character",
             .col.names = "character"
             )
         )

## Abstract interface, which is the parent of both classes
## defined in the above.
## Many functions in this package should operate on both classes.
## So we define this abstract class.
setClassUnion("db.data.frame", members = c("db.obj", "query.obj"))
