
## to be finished

## generic function
## @param db.obj - new relation in database (table or view)
## @param r.obj - R object which contains data for creating database object
setGeneric("as.data.frame.indb",
           function (db.obj, r.obj, ...) standardGeneric("as.data.frame.indb"),
           signature = c("db.obj", "r.obj"))

## create table from data.frame
setMethod("as.data.frame.indb",
          signature(db.obj = "character", r.obj = "data.frame"),
          function (db.obj, r.obj, row.names = TRUE, 
                    overwrite = FALSE, append = FALSE,
                    con.id = 1, order.by = NULL)
          {
              .db.writeTable(db.obj, r.obj, row.names, overwrite,
                             append, con.id)
              
          })
