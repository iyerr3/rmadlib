
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
