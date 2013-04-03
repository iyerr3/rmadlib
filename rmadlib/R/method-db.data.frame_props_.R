
setMethod ("dim",
           signature(x = "db.table"),
           function (x) {
               x@.dim
           })

## ------------------------------------------------------------------------

setMethod ("names",
           signature(x = "db.data.frame"),
           function (x) {
               x@.col.name
           })

## ------------------------------------------------------------------------

content <- function (x)
{
    if (! inherits(x, "db.obj"))
        stop("The argument must be a db.obj-class object!")

    x@.content
}
