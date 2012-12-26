### Not to be called by the users, so all functions start with a prefix of dot
### and thus are not exported.

### Database related utilities:

### ----------------------------------------------------------------
.db.connect.rpostgresql <- function(host, user, dbname, password)
{
    if (is.null(.localVars$drv$rpostgresql))
        .localVars$drv$rpostgresql <- DBI::dbDriver("PostgreSQL")
    
    n.db <- length(.localVars$db)
    db.connection <- RPostgreSQL::dbConnect(.localVars$drv$rpostgresql, host=host, user=user, dbname=dbname, password=password)
    .localVars$db[[n.db+1]] <- list(
                               con = db.connection,
                               host = host,
                               user = user,
                               dbname = dbname, # database name
                               rcon.pkg = "rpostgresql" # which R package is used to connected to database
                               )
    .localVars$con.type[["rpostgresql"]] <- c(.localVars$con.type[["rpostgresql"]], n.db+1)
    return(n.db + 1) # return the connection ID
}

.db.unloadDriver.rpostgresql <- function(drv)
{
    RPostgreSQL::dbUnloadDriver(drv)
}

### ----------------------------------------------------------------
.db.disconnect.rpostgresql <- function(con.id)
{
    RPostgreSQL::dbDisconnect (.localVars$db[[con.id]]$con)
}

### ----------------------------------------------------------------
.db.sendQuery.rpostgresql <- function(con.id, query)
{
    RPostgreSQL::dbSendQuery(.localVars$db[[con.id]]$con, query)
}

### ----------------------------------------------------------------
.db.getQuery.rpostgresql <- function(con.id, query)
{
    RPostgreSQL::dbGetQuery(.localVars$db[[con.id]]$con, query)
}

### ----------------------------------------------------------------
.db.listTables.rpostgresql <- function(con.id)
{
    RPostgreSQL::dbListTables(.localVars$db[[con.id]]$con)
}

### ----------------------------------------------------------------
.db.existsTable.rpostgresql <- function(con.id, table)
{
    RPostgreSQL::dbExistsTable(.localVars$db[[con.id]]$con, table)
}

### ----------------------------------------------------------------
.db.listColumnNames.rpostgresql <- function(con.id, table)
{
    if (!.db.existsTable.rpostgresql(con.id, table))
        stop("No such table!")

    RPostgreSQL::dbListFields(.localVars$db[[con.id]]$con, table)
}
