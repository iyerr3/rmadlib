## Execute on attach

## define local variables

.onAttach <- function(libname, pkgname)
{
    ## manage database connections
    .localVars$db <- list()
    
    ## R connection package
    ## right now, support RODBC and RPostgreSQL
    ## set up records for which connection ID belongs to which connection type
    ## source("supported-connections.R", local = TRUE)
    .localVars$con.type <- list()
    for (i in seq(along=.supported.connections))
        .localVars$con.type[[tolower(.supported.connections[i])]] <- integer(0)

    ## create backups for all functions that might be hard overridden
    for (i in seq(along = .localConst.hard.override.funcs))
    {
        pkg.splits <- strsplit(.localConst.hard.override.funcs[i], "::")[[1]]
        pkg.name <- pkg.splits[1]
        func.name <- pkg.splits[2]
        ## for example, assign base::data.frame to rmadlib::.origin.base.data.frame as a backup
        assign(paste(".origin.", pkg.name, ".", func.name, sep=""),
               eval(parse(text = .localConst.hard.override.funcs[i])),
               envir = as.environment(paste("package:", .this.pkg.name, sep="")))
    }
}
