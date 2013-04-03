
## ------------------------------------------------------------------------
## create a R object that points to something inside the database
## ------------------------------------------------------------------------

setGeneric("db.data.frame",
           def = function (x, ...) standardGeneric("db.data.frame"),
           signature = c("x"))

## ------------------------------------------------------------------------

setMethod("db.data.frame",
          signature(x = "character"),
          def = function (x, conn.id, id.col) .method.db.data.frame.1(x, conn.id, id.col),
          valueClass = "db.obj")

## ------------------------------------------------------------------------

.method.db.data.frame.1 <- function (x, conn.id, id.col)
{
    if (missing(conn.id)) conn.id <- 1 # default value in S4 context
    if (missing(id.col)) id.col <- NULL # no ID column

    table <- .db.obj.info(x) # a vector (schema_name, table_name)
    if (!.is.table.or.view(table, conn.id))
        stop("No such table or view!")

    if (.is.view(table, conn.id))
    {
        ## view
        res <- new("db.obj.view",
                   .name = table,
                   .conn.id = conn.id)
    }
    else
    {
        ## table
        res <- new("db.obj.table",
                   .name = table,
                   .conn.id = conn.id,
                   .id.col = id.col)
        
        ## compute dim
        col.num <- length(res@.col.names)
        row.num <- .db.getQuery(paste("select count(*) from", x), conn.id)
        res@.dim <- c(row.num$count, col.num)
    }

    col.info <- .db.getQuery(paste("select column_name, data_type, udt_name from information_schema_columns where table_name =", table[2], "and table_schema =", table[1]), conn.id)

    res@.col.name <- col.info$column_name
    res@.col.data_type <- col.info$data_type
    res@.col.udt_name <- col.info$udt_name

    ## table type (local temp)
    tbl.type <- .db.getQuery(paste("select table_type from information_schema.tables where table_name =", table[2], "and table_schema =", table[1]), conn.id)
    res$.table.type <- tbl.type

    return (res)
}

## ------------------------------------------------------------------------

setMethod("db.data.frame",
          signature(x = "query.obj"),
          def = function (x) .method.db.data.frame.2(x),
          valueClass = "db.obj")

## ------------------------------------------------------------------------

.method.db.data.frame.2 <- function (x)
{
    ## convert query.obj into db.obj
}
