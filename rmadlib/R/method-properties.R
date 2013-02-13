
## define function to get dimensions of a data table

## dimension of the table
setMethod("dim",
          signature(x = "indb.created"),
          function (x)
          {
              x@.dim
          }
          )

setMethod("colnames",
          signature(x = "indb.created"),
          function (x)
          {
              x@.col.names
          })

## name for indb.created object
setGeneric("name",
           def = function (x) standardGeneric("name"),
           signature = "x")
setMethod("name",
          signature(x = "indb.created"),
          function (x)
          {
              x@.name
          })

## type of the database object (table or view)
setGeneric("type",
           def = function (x) standardGeneric("type"),
           signature = "x")
setMethod("type",
          signature(x = "indb.created"),
          function (x)
          {
            x@.type
          })

## connection ID for indb.created object
setGeneric("con.id",
           def = function (x) standardGeneric("con.id"),
           signature = "x")
setMethod("con.id",
          signature(x = "indb.created"),
          function (x)
          {
              x@.con.id
          })

setGeneric("id.col",
           def = function (x) standardGeneric("id.col"),
           signature = "x")
setMethod("id.col",
          signature(x = "indb.created"),
          function (x)
          {
              x@.id.col
          })
