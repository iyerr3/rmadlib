
## to be finished

## data.frame.indb <- function(x, ...) {}

setGeneric("data.frame.indb",
           def = function (x, ...) standardGeneric("data.frame.indb"),
           signature = "x")

setMethod("data.frame.indb",
          signature(x = "character"),
          function (x, con.id = 1, order.by = "")
          {
              dfi <- new("indb.created",
                         .table = x,
                         .con.id = con.id,
                         .order.by = order.by,
                         .dim = numeric(0))

              dfi@.col.names <- db.listColumnNames(table = x, con.id = con.id)

              ## compute dim
              col.num <- length(dfi@.col.names)
              row.num <- db.getQuery(query = paste("select count(*) from", x), con.id = con.id)
              dfi@.dim <- c(row.num[,1], col.num)
              
              return (dfi)
          })


