

## to be finished

## data.frame.indb <- function(x, ...) {}

setGeneric("data.frame.indb",
           def = function (x, ...) standardGeneric("data.frame.indb"),
           signature = "x")

setMethod("data.frame.indb",
          signature(x = "character"),
          function (x, con.id = 1, id.col = character(0))
          {
              if (.is.table(x, con.id) && ! .is.view(x, con.id))
              {
                  ## x is a table in database
                  dfi <- new("indb.created",
                             .table = x,
                             .con.id = con.id,
                             .dbname = dbname(con.id),
                             .host = host(con.id),
                             .con.pkg = con.pkg(con.id),
                             .id.col = id.col,
                             .dim = numeric(0))
                  
                  dfi@.col.names <- db.listColumnNames(table = x, con.id = con.id)
                  
                  ## compute dim
                  col.num <- length(dfi@.col.names)
                  row.num <- db.getQuery(query = paste("select count(*) from", x), con.id = con.id)
                  dfi@.dim <- c(row.num[,1], col.num)
              }
              else if (.is.view(x, con.id))
              {
                  ## x is a view in database
              }
              return (dfi)
          })


