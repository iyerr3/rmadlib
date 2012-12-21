## Not to be called by the users, so all functions start with a prefix of dot
## and thus are not exported.

## Database related utilities:

## ----------------------------------------------------------------
.db.connect.rpostgresql <- function(host, user, dbname, password = "")
{
    if (!is.arg.string(host) ||
        !is.arg.string(user) ||
        !is.arg.string(dbname) ||
        !is.arg.string(password))
        stop("Host, user, dbname and password (could be an empty string) should all be strings!")

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

## ----------------------------------------------------------------
.db.disconnect.rpostgresql <- function(con.id = .localVars$con.type[["rpostgresql"]][1])
{
    ## check whether this connection exists
    if (!is.con.id.valid(con.id))
        stop("There is no such connection!")
    
    RPostgreSQL::dbDisconnect (.localVars$db[[con.id]]$con)
    .localVars$db[[con.id]] <- NULL
    .localVars$con.type[["rpostgresql"]] <- .localVars$con.type[["rpostgresql"]][-which(.localVars$con.type[["rpostgresql"]]==con.id)]
}

## ----------------------------------------------------------------
.db.sendQuery.rpostgresql <- function(con.id = .localVars$con.type[["rpostgresql"]][1], query)
{
    if (!is.con.id.valid(con.id))
        stop("There is no such connection!")
    
    RPostgreSQL::dbSendQuery(.localVars$db[[con.id]]$con, query)
}

.db.getQuery.rpostgresql <- function(con.id = .localVars$con.type[["rpostgresql"]][1], query)
{
    if (!is.con.id.valid(con.id))
        stop("There is no such connection!")

    RPostgreSQL::dbGetQuery(.localVars$db[[con.id]]$con, query)
}
