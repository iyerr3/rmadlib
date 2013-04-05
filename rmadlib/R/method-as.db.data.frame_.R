
## ------------------------------------------------------------------------
## Convert other R objects into db.data.frame
## ------------------------------------------------------------------------

setGeneric ("as.db.data.frame",
            def = function (x, ...) standardGeneric("as.db.data.frame"),
            signature = "x")

## ------------------------------------------------------------------------

## put a data.frame into a db.data.frame
setMethod ("as.db.data.frame",
           signature (x = "data.frame"),
           def = function (
           x, table.name, conn.id = 1, add.row.names = FALSE,
           id.col = character(0), distributed.by = NULL,
           is.temp = FALSE, ...)
           .method.as.db.data.frame.1(x, table.name, conn.id,
                                      add.row.names, id.col,
                                      distributed.by, is.temp, ...),
           valueClass = "db.data.frame")

## ------------------------------------------------------------------------

## put a file into a db.data.frame
## put a data.frame into a db.data.frame
setMethod ("as.db.data.frame",
           signature (x = "character"),
           def = function (
           x, table.name, conn.id = 1, add.row.names = FALSE,
           id.col = character(0), distributed.by = NULL,
           is.temp = FALSE, ...)
           .method.as.db.data.frame.1(x, table.name, conn.id,
                                      add.row.names, id.col,
                                      distributed.by, is.temp, ...),
           valueClass = "db.data.frame")

## ------------------------------------------------------------------------

.method.as.db.data.frame.1 <- function (x, table.name, conn.id = 1, add.row.names = FALSE,
                                        id.col = character(0), distributed.by = NULL,
                                        is.temp = FALSE, ...) {
    print(id.col)
    print(.is.arg.string(id.col))
    if (!.is.arg.string(id.col)) stop("ID column name must be a string!")
    if (length(nchar(id.col)) > 0 &&
        id.col == "row.names" && !add.row.names)
        stop("If you want to use row.names as id.col, you have to set row.names as TRUE!")
    ## argument default, and checking
    ## if (missing(conn.id)) conn.id <- 1
    if (!.is.conn.id.valid(conn.id))
        stop("There is no such a connection!")
    if (!.is.arg.string(table.name) ||
        nchar(table.name) == 0)
        stop("The table name is not quite right!")
    ## if (missing(distributed.by)) distributed.by <- NULL
    ## if (missing(is.temp)) is.temp <- FALSE

    table <- .db.analyze.table.name(table.name)
    if ((!is.temp && .db.existsTable(table, conn.id)) ||
        (is.temp && .db.existsTempTable(table, conn.id)[[1]]))
        stop("Table already exists!")
    
    .db.writeTable(table, x, add.row.names = add.row.names,
                   distributed.by = distributed.by,
                   is.temp = is.temp, ...)
    if (length(table) == 1 && !is.temp) {
        table_schema <- .db.getQuery("select current_schema()");
        table.str <- paste(table_schema, ".", table, sep = "")
    } else
        table.str <- table.name
    if (length(nchar(id.col)) != 0)
        .db.getQuery(paste("alter table ", table.str,
                           " add primary key (\"",
                           id.col, "\")", sep = ""))
    
    db.data.frame(table.name, conn.id, id.col)
}
