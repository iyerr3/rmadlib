
## Universal database connection utilities
## Multiple R connection packages are supported

.db.connect <- function(host, user, dbname, password = "",
                       con.pkg = "RPostgreSQL")
{
    ## available packages, to check whether RODBC and RPostgreSQL are
    ## already installed
    if (is.null(.localVars$installed.pkgs))
        .localVars$installed.pkgs <- tolower(attr(installed.packages(), "dimnames")[[1]])
    
    ## argument type check
    if (!.is.arg.string(host) ||
        !.is.arg.string(user) ||
        !.is.arg.string(dbname) ||
        !.is.arg.string(password) ||
        !.is.arg.string(con.pkg))
        stop("Host, user, dbname, password (could be an empty string) and the connection package should all be strings!")

    ## use one of the R connection package to connect to database
    con.pkg.name <- tolower(con.pkg)
    if (con.pkg.name %in% tolower(.supported.connections)) # make sure that the package is supported
    {
        i <- which(tolower(.supported.connections) == con.pkg.name)
        pkg.to.load <- .supported.connections[i]
        if (!(con.pkg.name %in% .localVars$installed.pkgs)) # if the package is not installed, install it
        {
            print(paste("Package ", pkg.to.load, " is going to be installed so that ", .this.pkg.name, " could connect to databases.\n\n", sep = ""))
            install.packages(pkgs = pkg.to.load, type = "source")
        }

        eval(parse(text = paste("library(", pkg.to.load, ")", sep = "")))
        command <- paste(".db.connect.", con.pkg.name, "(host=\"", host, "\", user=\"", user, "\", dbname=\"", dbname, "\", password=\"", password, "\")", sep = "")
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
.db.sendQuery <- function(query, con.id = 1)
{
    if (!.is.con.id.valid(con.id))
        stop("There is no such connection!")

    if (!.is.arg.string(query))
        stop("The query must be a string!")

    command <- paste(".db.sendQuery.", .localVars$db[[con.id]]$rcon.pkg, "(query=\"", query, "\", con.id=", con.id, ")", sep = "")
    eval(parse(text = command))
}

### ----------------------------------------------------------------
.db.getQuery <- function(query, con.id = 1)
{
    if (!.is.con.id.valid(con.id))
        stop("There is no such connection!")

    if (!.is.arg.string(query))
        stop("The query must be a string!")

    command <- paste(".db.getQuery.", .localVars$db[[con.id]]$rcon.pkg, "(query=\"", query, "\", con.id=", con.id, ")", sep = "")
    eval(parse(text = command))
}

### ----------------------------------------------------------------
.db.listTables <- function(con.id = 1)
{
    if (!.is.con.id.valid(con.id))
        stop("There is no such connection!")

    command <- paste(".db.listTables.", .localVars$db[[con.id]]$rcon.pkg, "(con.id=", con.id, ")", sep = "")
    eval(parse(text = command))
}

### ----------------------------------------------------------------
.db.existsTable <- function(table, con.id = 1)
{
    if (!.is.con.id.valid(con.id))
        stop("There is no such connection!")

    if (!.is.arg.string(table))
        stop("The table name must be a string!")

    command <- paste(".db.existsTable.", .localVars$db[[con.id]]$rcon.pkg, "(table=\"", table, "\", con.id=", con.id, ")", sep = "")
    eval(parse(text = command))
}

### ----------------------------------------------------------------
.db.listColumnNames <- function(table, con.id = 1)
{
    if (!.is.con.id.valid(con.id))
        stop("There is no such connection!")

    if (!.is.arg.string(table))
        stop("The table name must be a string!")

    command <- paste(".db.listColumnNames.", .localVars$db[[con.id]]$rcon.pkg,
                     "(table=\"", table, "\", con.id=", con.id, ")", sep = "")
    eval(parse(text = command))
}

## ------------------------------------------------------------------------
.db.writeTable <- function(table, dframe, row.names = TRUE, 
                           overwrite = FALSE, append = FALSE,
                           con.id = 1)
{
    if (! .is.con.id.valid(con.id))
        stop("There is no such connection!")

    if (! .is.arg.string(table))
        stop("The table name must be a string!")

    if (! is.data.frame(dframe))
        stop("The data must be in a data frame format!")

    command <- paste(".db.listColumnNames.", .localVars$db[[con.id]]$rcon.pkg,
                     "(table=\"", table, "\", dframe=", dframe,
                     ", row.names=", row.names, ", overwrite=", overwrite,
                     ", append=", append, ", con.id=", con.id, ")", sep = "")
    eval(parse(text = command))
}
