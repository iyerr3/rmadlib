
## to be finished

data.frame.indb <- function(x, ...) {}

setGeneric("data.frame.indb", signature = "x")

setMethod("data.frame.indb",
          signature(x = "character"),
          function (x, con.id = 1, order.by = "")
          {
              dfi <- new("indb.created",
                         table = x,
                         con.id = con.id,
                         order.by = order.by,
                         dim = numeric(0))
              ## compute dim
              row.num <- db.getQuery(con.id,
                                     paste("select count(*) from", x))
              col.num <- length(db.listColumnNames(con.id, x))
              dfi@dim <- c(row.num[,1], col.num)

              return (dfi)
          })


