
## ------------------------------------------------------------------------
## create a R object that points to something inside the database
## ------------------------------------------------------------------------

setGeneric("db.data.frame",
           def = function (x, conn.id, ...) standardGeneric("db.data.frame"),
           signature = c("x", "conn.id"))

## ------------------------------------------------------------------------

setMethod("db.data.frame",
          signature(x = "character", conn.id = "numeric"),
          def = function (x, conn.id) .method.db.data.frame.1(x, conn.id),
          valueClass = "db.obj")

## ------------------------------------------------------------------------

.method.db.data.frame.1 <- function (x, conn.id)
{
    if (missing(conn.id)) conn.id <- 1 # default value in S4 context

    
}
