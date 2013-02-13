
### print indb.created and indb.uncreated

setGeneric("print", signature = "x")

setMethod("print",
          signature(x = "indb.created"),
          function (x)
          {
              cat(paste("Table ", x@table, " in database ", dbname(con.id(x)), "\n", sep = ""))
              cat(paste("Host: ", host(con.id(x)), "\n", sep = ""))
              cat(paste("Connected with package ", con.pkg(con.id(x)), "\n", sep = ""))
          })

setMethod("show",
          signature(object = "indb.created"),
          function (object)
          {
              print(object)
          })
