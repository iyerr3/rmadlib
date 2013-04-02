## Execute on attach

## define local variables

.onAttach <- function(libname, pkgname)
{
    ## manage database connections
    .localVars$db <- list()
    
    ## R connection package
    ## right now, only support RPostgreSQL
    ## set up records for which connection ID belongs to which connection type
    ## source("supported-connections.R", local = TRUE)
    .localVars$conn.type <- list()
    for (i in seq(along=.supported.connections))
        .localVars$conn.type[[tolower(.supported.connections[i])]] <- integer(0)
}
