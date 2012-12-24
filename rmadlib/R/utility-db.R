
.db.connect <- function(host, user, dbname, password = "",
                        con.pkg = "RPostgreSQL")
{
    ## argument type check
    if (!.is.arg.string(host) ||
        !.is.arg.string(user) ||
        !.is.arg.string(dbname) ||
        !.is.arg.string(password) ||
        !.is.arg.string(con.pkg))
        stop("Host, user, dbname, password (could be an empty string) and the connection package should all be strings!")

    ## if the package is not installed, install it
    con.pkg.name <- tolower(con.pkg)
    if (con.pkg.name %in% tolower(.supported.connections)) # make sure that the package is supported
    {
        if (!(con.pkg.name %in% .localConst.installed.pkgs))
        {
            i <- which(tolower(.supported.connections) == con.pkg.name)
            pkg.to.install <- .supported.connections[i]
            print(paste("Package ", pkg.to.install, " is going to be installed so that ", .this.pkg.name, " could connect to databases.\n\n", sep = ""))
            install.packages(pkgs = pkg.to.install, type = "source")
        }

        command <- paste(".db.connect.", con.pkg.name, "(host=", host, "user=", user, "dbname=", dbname, "password=", password, ")", sep = "")
        result <- eval(parse(text = command))
        return (paste("Created a connection to database with ID ", result))
    }
    else
    {
        stop(paste("Right now, only ", .supported.connections, " is supported to connected to database.\n", sep = ""))
    }
}

### ----------------------------------------------------------------    
.db.disconnect <- function(con.id = 1)
{
    ## check whether this connection exists
    if (!.is.con.id.valid(con.id))
        stop("There is no such connection!")

    rcon.pkg <- .localVars$db[[con.id]]$rcon.pkg
    command <- paste(".db.disconnect.", rcon.pkg, "(con.id=", con.id, ")", sep = "")
    eval(parse(text = command))
    .localVars$db[[con.id]] <- NULL
    .localVars$con.type[[rcon.pkg]] <- .localVars$con.type[[rcon.pkg]][-which(.localVars$con.type[[rcon.pkg]]==con.id)]
}

### ----------------------------------------------------------------
.db.sendQuery <- function(con.id = 1, query)
{
    if (!.is.con.id.valid(con.id))
        stop("There is no such connection!")

    ## Users will never directly use string queries
    ## The exported functions will make sure the query is always strings
    ## no need to check 
    ## if (!.is.arg.string(query))
    ##     stop("The query must be a string!")

    command <- paste(".db.sendQuery.", .localVars$db[[con.id]]$rcon.pkg, "(con.id=", con.id, "query=", query, ")", sep = "")
    eval(parse(text = command))
}

### ----------------------------------------------------------------
.db.getQuery <- function(con.id = 1, query)
{
    
}
