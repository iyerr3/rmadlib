## Execute on attach

## define local variables

.onAttach <- function(libname, pkgname)
{
    .localVars <- new.env()
    
    ## R connection package
    ## right now, support RODBC and RPostgreSQL
    .localVars$rcon.pkg <- character(0) 
    .localVars$db <- list()

    ## set up records for which connection ID belongs to which connection type
    source("supported-connections.R", local = TRUE)
    .localVars$con.type <- list()
    for (i in seq(along=.supported.connections))
        .localVars$con.type[[.supported.connections[i]]] <- integer(0)

    ## create back-ups for functions that might be overridden
    source("overridden-funcs.R", local = TRUE)
    for (i in seq(along=.local.const.overridden))
    {
        pkg.splits <- strsplit(.localConst.overridden[i], "::")[[1]]
        pkg.name <- pkg.splits[1]
        func.name <- pkg.splits[2]
        ##
        assign(paste(".origin.", pkg.name, ".", func.name, sep=""),
               eval(parse(text = .localConst.overridden[i])),
               envir = as.environment("package:rmadlib"))
    }
}
