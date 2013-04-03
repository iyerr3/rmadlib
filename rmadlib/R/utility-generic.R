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
    is.character(arg)
}

## ------------------------------------------------------------------------

.is.conn.id.valid <- function (conn.id)
{
    !is.na(conn.id) && conn.id >= 1 && conn.id <= length(.localVars$db)
}

## ------------------------------------------------------------------------

.db.obj.info <- function (db.obj_name, conn.id = 1)
{
    parts <- strsplit(db.obj_name, "\\.")[[1]]
    if (length(parts) == 2) {
        table_schema <- parts[1]
        table_name <- parts[2]
    } else if (length(parts) == 1) {
        table_name <- parts[1]
        table_schema <- .db.getQuery("select current_schema()", conn.id)
    } else {
        stop("The database object name is not valid!")
    }
    return (c(table_schema, table_name))
}

## ------------------------------------------------------------------------

## Is the object in database a table?
.is.table.or.view <- function (table, conn.id = 1)
{
    pick <- .db.getQuery(paste("select count(*) from information_schema.tables where table_name = '",
                               table[2],
                               "' and table_schema ='", table[1], "'", sep = ""), conn.id)
    if (pick == 1)
        return (TRUE)
    else
        return (FALSE)
}

## ------------------------------------------------------------------------

## Is the object in database a view?
.is.view <- function (table, conn.id = 1)
{
    pick <- .db.getQuery(paste("select count(*) from information_schema.views where table_name = '",
                               table[2],
                               "' and table_schema = '", table[1], "'", sep = ""), conn.id)
    if (pick == 1)
        return (TRUE)
    else
        return (FALSE)
}
