
## Small utility functions to extract information
## about a given connection

dbname <- function (con.id = 1)
{
    .localVars$db[[con.id]]$dbname
}

host <- function (con.id = 1)
{
    .localVars$db[[con.id]]$host
}

user <- function (con.id = 1)
{
    .localVars$db[[con.id]]$user
}

con.pkg <- function (con.id = 1)
{
    pkg <- .localVars$db[[con.id]]$rcon.pkg
    i <- which(tolower(.supported.connections) == pkg)
    .supported.connections[i]
}
