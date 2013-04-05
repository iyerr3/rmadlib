## Not to be called by the users, so all functions start with a prefix of dot
## and thus are not exported.

## Check arguments of function to make sure that they are legal

## ------------------------------------------------------------------------

## convert {...} string into an array
.db.str2vec <- function (str, type = "double")
{
    elm <- regmatches(str, gregexpr("[^,\"\\s\\{\\}]+|\"(\\\"|[^\"])*\"", str, perl=T))[[1]]
    if (type == "character")
        return (elm)
    else if (type == "integer")
        return (as.integer(elm))
    else if (type == "logical")
        return (as.logical(toupper(elm)))
    else
        return (as.numeric(elm))
}

## ------------------------------------------------------------------------

.is.arg.string <- function (arg)
{
    return (!is.null(arg) &&
            is.character(arg))
}

## ------------------------------------------------------------------------

.is.conn.id.valid <- function (conn.id)
{
    return (length(conn.id) != 0 &&
            !is.null(conn.id) &&
            !is.na(conn.id) && 
            conn.id >= 1 &&
            length(.localVars$conn.id) != 0 &&
            conn.id %in% .localVars$conn.id[,1])
}

## ------------------------------------------------------------------------

## It is a little bit more complicated when schema name is not given.
## The table might be a temporary table, or a normal table. 
## .db.obj.info <- function (db.obj_name, conn.id = 1)
## {
##     parts <- strsplit(db.obj_name, "\\.")[[1]]
##     if (length(parts) == 2) {
##         table_schema <- parts[1]
##         table_name <- parts[2]
##     } else if (length(parts) == 1) {
##         table_name <- parts[1]
   
##         schemas <- .db.str2vec(.db.getQuery("select current_schemas(True)", conn.id),
##                                type = "character")
##         table_schema <- NULL
##         for (schema in schemas)
##         {
##             if (.db.existsTable(c(schema, table_name), conn.id)) {
##                 table_schema <- schema
##                 break
##             }
##         }
        
##         ## No such table, going to create a new one
##         ## usually in public
##         if (is.null(table_schema))
##             table_schema <- .db.getQuery("select current_schema()", conn.id)        
##     } else {
##         stop("The database object name is not valid!")
##     }
##     return (c(table_schema, table_name))
## }

## ------------------------------------------------------------------------

## simply return the most direct answer
.db.analyze.table.name <- function (name)
{
    parts <- strsplit(name, "\\.")[[1]]
    l <- length(parts)
    if (l != 1 && l != 2)
        stop("The database object name is not valid!")
    return (parts)
}

.db.table.schema.str <- function (table)
{
    l <- length(table)
    if (l == 2)
        return (paste("table_name = '", table[2], "' and table_schema = '",
                      table[1], "'", sep = ""))
    else if (l == 1)
        return (paste("table_name = '", table, "'", sep = ""))
}

## ------------------------------------------------------------------------

## Is the object in database a table?
.is.table.or.view <- function (table, conn.id = 1)
{
    pick <- .db.getQuery(paste("select count(*) from information_schema.tables where ",
                               .db.table.schema.str(table), sep = ""), conn.id)
    if (pick == 1)
        return (TRUE)
    else
        return (FALSE)
}

## ------------------------------------------------------------------------

## Is the object in database a view?
.is.view <- function (table, conn.id = 1)
{
    pick <- .db.getQuery(paste("select count(*) from information_schema.views where ",
                               .db.table.schema.str(table), sep = ""), conn.id)
    if (pick == 1)
        return (TRUE)
    else
        return (FALSE)
}

## ------------------------------------------------------------------------

.unique.string <- function ()
{
    r1 <- sample(seq_len(100000000), 1)
    r2 <- unclass(as.POSIXct(strptime(date(),"%c")))[1]
    r3 <- r2 %% r1
    paste("__madlib_temp_", r1, "_", r2, "_", r3, "__", sep = "")
}

## ------------------------------------------------------------------------
