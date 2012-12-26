
## define function to get dimensions of a data table

setMethod("dim",
          signature(x = "indb.created"),
          function (x)
          {
              x@dim
          }
          )
