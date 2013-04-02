
## Small utility functions to extract information
## about a given connection

dbname <- function (conn.id = 1)
{
    if (!.is.conn.id.valid(conn.id))
        stop("There is no such connection!")
    .localVars$db[[conn.id]]$dbname
}

host <- function (conn.id = 1)
{
    if (!.is.conn.id.valid(conn.id))
        stop("There is no such connection!")
    .localVars$db[[conn.id]]$host
}

user <- function (conn.id = 1)
{
    if (!.is.conn.id.valid(conn.id))
        stop("There is no such connection!")
    .localVars$db[[conn.id]]$user
}

conn.pkg <- function (conn.id = 1)
{
    if (!.is.conn.id.valid(conn.id))
        stop("There is no such connection!")
    pkg <- .localVars$db[[conn.id]]$conn.pkg
    i <- which(tolower(.supported.connections) == pkg)
    .supported.connections[i]
}
