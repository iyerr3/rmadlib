
## ------------------------------------------------------------------------
## Convert other R objects into db.data.frame
## ------------------------------------------------------------------------

setGeneric ("as.db.data.frame",
            def = function (x, ...) standardGeneric("as.db.data.frame"),
            signature = "x")

## ------------------------------------------------------------------------

setMethod ("as.db.data.frame",
           signature (x = "data.frame"),
           def = function (x, table.name, conn.id = 1, id.col = "row.names",
           distributed.by = NULL, is.temp = FALSE) {
               ## argument default, and checking
               ## if (missing(conn.id)) conn.id <- 1
               if (!.is.conn.id.valid(conn.id))
                   stop("There is no such a connection!")
               if (!.is.arg.string(table.name) ||
                   nchar(table.name) != 0)
                   stop("The table name is not quite right!")
               ## if (missing(distributed.by)) distributed.by <- NULL
               ## if (missing(is.temp)) is.temp <- FALSE
               if (!.is.arg.string(id.col)) stop("ID column name must be a string!")
               
               table <- .db.analyze.table.name(table.name)
               if ((!is.temp && .db.existsTable(table, conn.id)) ||
                   (is.temp && .db.existsTempTable(table, conn.id)))
                   stop("Table already exists!")
               
               z <- cbind(attr(x, "row.names"), x)
               colnames(z) <- c("row.names", names(x))
               .db.writeTable(table, z, row.names = TRUE, distributed.by = distributed.by,
                              is.temp = is.temp)
               
               db.data.frame(table.name, conn.id, id.col)
           })
