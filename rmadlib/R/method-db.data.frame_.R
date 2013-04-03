
## ------------------------------------------------------------------------
## create a R object that points to something inside the database
## ------------------------------------------------------------------------

setGeneric ("db.data.frame",
            def = function (x, ...) standardGeneric("db.data.frame"),
            signature = c("x"))

## ------------------------------------------------------------------------

setMethod ("db.data.frame",
           signature(x = "character"),
           def = function (x, conn.id, id.col) .method.db.data.frame.1(x, conn.id, id.col),
           valueClass = "db.data.frame")

## ------------------------------------------------------------------------

.method.db.data.frame.1 <- function (x, conn.id, id.col)
{
    if (missing(conn.id)) conn.id <- 1 # default value in S4 context
    if (missing(id.col)) id.col <- character(0) # no ID column

    if (!.is.conn.id.valid(conn.id))
        stop("There is no such a connection to any database!")

    table <- .db.obj.info(x) # a vector (schema_name, table_name)
    if (!.is.table.or.view(table, conn.id))
        stop("No such table or view!")

    if (.is.view(table, conn.id))
    {
        ## view
        res <- new("db.view",
                   .name = table,
                   .content = x,
                   .conn.id = conn.id)
    }
    else
    {
        ## table
        res <- new("db.table",
                   .name = table,
                   .content = x,
                   .conn.id = conn.id,
                   .id.col = id.col)
        
        ## compute dim
        col.num <- length(res@.col.name)
        row.num <- .db.getQuery(paste("select count(*) from", x), conn.id)
        res@.dim <- c(row.num$count, col.num)
    }

    col.info <- .db.getQuery(paste("select column_name, data_type, udt_name from information_schema.columns where table_name = '", table[2], "' and table_schema = '", table[1], "'", sep = ""), conn.id)

    res@.col.name <- col.info$column_name
    res@.col.data_type <- col.info$data_type
    res@.col.udt_name <- col.info$udt_name

    ## table type (local temp)
    tbl.type <- .db.getQuery(paste("select table_type from information_schema.tables where table_name = '", table[2], "' and table_schema = '", table[1], "'", sep = ""), conn.id)
    res@.table.type <- tbl.type$table_type

    return (res)
}

