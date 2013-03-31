## Execute on unloading

## release local variables

.onUnload <- function(libpath)
{
    ## close all unclosed database connections
    for (i in seq(along=.localVars$db))
        db.disconnect(con.id = i)

    ## also unload all db connection drivers
    pkg.names <- names(.localVars$drv)
    for (pkg in pkg.names)
    {
        command <- paste(".db.unloadDriver.", drv, "(drv=", .localVars$drv[[pkg]], ")", sep = "")
        eval(parse(text = command))
    }
}
