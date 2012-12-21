## Execute on unloading

## release local variables

.onUnload <- function(libpath)
{
    ## close all unclosed database connections
    for (i in seq(along=.localVars$db))
        if (.localVars$db[[i]]$rcon.pkg == "rpostgresql")
            RPostgreSQL::dbDisconnect (.localVars$db[[i]]$connection)
        else if (.localVars$db[[i]]$rcon.pkg == "rodbc")
            RODBC::odbcClose (.localVars$db[[i]]$connection)
}
