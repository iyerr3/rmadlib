
### ------------------------------------------------------------------------
### Some utility functions to get info about data table
### ------------------------------------------------------------------------

## define function to get dimensions of a data table

## dimension of the table
setMethod("dim",
          signature(x = "db.obj"),
          function (x)
          {
              x@.dim
          }
          )

setMethod("colnames",
          signature(x = "db.obj"),
          function (x)
          {
              x@.col.names
          })

## name for db.obj object
setGeneric("name",
           def = function (x) standardGeneric("name"),
           signature = "x")
setMethod("name",
          signature(x = "db.obj"),
          function (x)
          {
              x@.name
          })

## type of the database object (table or view)
setGeneric("type",
           def = function (x) standardGeneric("type"),
           signature = "x")
setMethod("type",
          signature(x = "db.obj"),
          function (x)
          {
            x@.type
          })

## connection ID for db.obj object
setGeneric("con.id",
           def = function (x) standardGeneric("con.id"),
           signature = "x")
setMethod("con.id",
          signature(x = "db.obj"),
          function (x)
          {
              x@.con.id
          })

setGeneric("id.col",
           def = function (x) standardGeneric("id.col"),
           signature = "x")
setMethod("id.col",
          signature(x = "db.obj"),
          function (x)
          {
              x@.id.col
          })
