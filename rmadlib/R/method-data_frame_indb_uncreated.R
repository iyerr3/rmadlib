
setGeneric("data.frame.indb.uncreated",
           def = function (x, ...) standardGeneric("data.frame.indb.uncreated"),
           signature = "x")

setMethod("data.frame.indb.uncreated",
          signature(x = "character"), # x is a query
          function (x, con.id = 1)
          {
              dfiu <- new("indb.uncreated",
                          .query = x,
                          .con.id = 1)

              dfiu@.dbname <- dbname(con.id)
              dfiu@.host <- host(con.id)
              dfiu@.con.pkg <- con.pkg(con.id)
          })
