## Not to be called by the users, so all functions start with a prefix of dot
## and thus are not exported.

## Check arguments of function to make sure that they are legal

## ----------------------------------------------------------------

.is.arg.string <- function (arg)
{
    is.character(arg)
}

.is.conn.id.valid <- function (conn.id)
{
    !is.na(conn.id) && conn.id >= 1 && conn.id <= length(.localVars$db)
}

.db.obj.info <- function (db.obj_name, conn.id = 1)
{
    parts <- strsplit(db.obj_name, "\\.")[[1]]
    if (length(parts) == 2) {
        table_schema <- parts[1]
        table_name <- parts[2]
    } else if (length(parts) == 1) {
        table_name <- parts[1]
        table_schema <- dbGetQuery(.localVars$db[[conn.id]]$con,
                                   "select current_schema()")
    } else {
        stop("The database object name is not valid!")
    }
    return list(table_name = table_name,
                table_schema = table_schema)
}

.is.table <- function (db.obj_name, conn.id = 1)
{
    table.info <- .db.obj.info(db.obj_name, conn.id)
    pick <- dbGetQuery(.localVars$db[[conn.id]]$con,
                       paste("select count(*) from information_schema.tables where table_name =",
                             table.info$table_name,
                             "and table_schema =", table.info$table_schema))
    if (pick == 1)
        return TRUE
    else
        return FALSE
}

.is.view <- function (db.obj_name, conn.id = 1)
{
    table.info <- .db.obj.info(.db.obj_name, conn.id)
    pick <- dbGetQuery(.localVars$db[[conn.id]]$con,
                       paste("select count(*) from information_schema.views where table_name =",
                             table.info$table_name,
                             "and table_schema =", table.info$table_schema))
    if (pick == 1)
        return TRUE
    else
        return FALSE
}
