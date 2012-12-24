### Not to be called by the users, so all functions start with a prefix of dot
### and thus are not exported.

### Database related utilities:

### ----------------------------------------------------------------
.db.connect.rpostgresql <- function(host, user, dbname, password)
{
    drv <- RPostgreSQL::dbDriver("PostgreSQL")
    n.db <- length(.localVars$db)
    db.connection <- RPostgreSQL::dbConnect(drv, host=host, user=user, dbname=dbname, password=password)
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
    if (!.is.con.id.valid(con.id))
        stop("There is no such connection!")

    RPostgreSQL::dbGetQuery(.localVars$db[[con.id]]$con, query)
}
