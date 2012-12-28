
### print indb.created and indb.uncreated

setGeneric("print", signature = "x")

setMethod("print",
          signature(x = "indb.created"),
          function (x)
          {
              cat(paste("Table ", x@table, " in database ",
                                   .localVars$db[[x@con.id]]$dbname, "\n", sep = ""))
              cat(paste("User name: ", .localVars$db[[x@con.id]]$user, "\n", sep = ""))
              cat(paste("Host: ", .localVars$db[[x@con.id]]$host, "\n", sep = ""))
          })

setMethod("show",
          signature(object = "indb.created"),
          function (object)
          {
              print(object)
          })
