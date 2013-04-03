
## ------------------------------------------------------------------------
## Universal database connection utilities
## Multiple R connection packages are supported

## Note: Internal functions do not need argument check
## only functions that are exposed to the users need the check
## ------------------------------------------------------------------------

## connect to a database using a specific R package
## Right now, only RPostgreSQL is supported
## If the connection package is not installed, it will
## be automatically installed
## A driver will be automatically created for connection package
db.connect <- function (host, user, dbname, password = "", port = "",
                        conn.pkg = "RPostgreSQL")
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
        !.is.arg.string(conn.pkg))
        stop("Host, user, dbname, password (could be an empty string) and the connection package should all be strings!")

    ## use one of the R connection package to connect to database
    conn.pkg.name <- tolower(conn.pkg)
    if (conn.pkg.name %in% tolower(.supported.connections)) # make sure that the package is supported
    {
        i <- which(tolower(.supported.connections) == conn.pkg.name)
        pkg.to.load <- .supported.connections[i]
        if (!(conn.pkg.name %in% .localVars$installed.pkgs)) # if the package is not installed, install it
        {
            print(paste("Package ", pkg.to.load, " is going to be installed so that ",
                        .this.pkg.name, " could connect to databases.\n\n", sep = ""))
            install.packages(pkgs = pkg.to.load, type = "source")
        }

        eval(parse(text = paste("library(", pkg.to.load, ")", sep = "")))
        command <- paste(".db.connect.", conn.pkg.name, "(host=\"", host,
                         "\", user=\"", user, "\", dbname=\"", dbname,
                         "\", password=\"", password, "\", port=", port,
                         ")", sep = "")
        result <- eval(parse(text = command))
        cat(paste("Created a connection to database with ID", result, "\n"))
        return (result)
    }
    else
    {
        stop(paste("Right now, only ", .supported.connections, " is supported to connected to database.\n", sep = ""))
    }
}

## ------------------------------------------------------------------------ 

## disconnect a connection
db.disconnect <- function (conn.id = 1, verbose = TRUE)
{
    ## check whether this connection exists
    if (!.is.conn.id.valid(conn.id))
        stop("There is no such connection!")

    conn.pkg <- .localVars$db[[conn.id]]$conn.pkg
    command <- paste(".db.disconnect.", conn.pkg, "(conn.id=", conn.id, ")", sep = "")
    res <- eval(parse(text = command))
    if (res)
    {
        .localVars$db[[conn.id]] <- NULL
        .localVars$conn.type[[conn.pkg]] <- .localVars$conn.type[[conn.pkg]][-which(.localVars$conn.type[[conn.pkg]]==conn.id)]

        if (verbose)
            cat(paste("Connection", conn.id, "is disconnected!\n"))
    }
    else
    {
        cat("There was a problem and the connection cannot be disconnected")
    }

    return (res)
}

## ------------------------------------------------------------------------

## List all connection info
db.list <- function ()
{
    n.conn <- length(.localVars$db)
    cat("\nDatabase Connection Info\n")
    if (n.conn == 0)
    {
        cat("\n## -------------------------------\n")
        cat("******** No database connections! ********\n\n")
    }
    else
    {
        for (i in seq(n.conn))
        {
            cat("\n## -------------------------------\n")
            cat(paste("[Connection ID ", i, "]\n", sep = ""))
            cat(paste("Host :     ", .localVars$db[[i]]$host, "\n", sep = ""))
            cat(paste("User :     ", .localVars$db[[i]]$user, "\n", sep = ""))
            cat(paste("Database : ", .localVars$db[[i]]$dbname, "\n", sep = ""))
            
            pkg <- .localVars$db[[i]]$conn.pkg
            id <- which(tolower(.supported.connections) == pkg)
            cat(paste("Conn pkg : ", .supported.connections[id], "\n", sep = ""))
        }
        cat("\n")
    }
}

## ------------------------------------------------------------------------
## All the following function are used inside the package only
## ------------------------------------------------------------------------

## ------------------------------------------------------------------------

## fetch the result of sendQuery
.db.fetch <- function (res, n = 500)
{
    conn.id <- res$conn.id
    command <- paste(".db.fetch.", .localVars$db[[conn.id]]$conn.pkg, "(res = res$res, n = n)", sep = "")
    eval(parse(text = command))
}

## ------------------------------------------------------------------------

## unload driver for a specific connection package
.db.unloadDriver <- function (pkg)
{
    command <- paste(".db.unloadDriver.", pkg, "(drv=",
                     .localVars$drv[[pkg]], ")", sep = "")
    eval(parse(text = command))
}

## ------------------------------------------------------------------------

.db.sendQuery <- function (query, conn.id = 1)
{
    command <- paste(".db.sendQuery.", .localVars$db[[conn.id]]$conn.pkg,
                     "(query=query, conn.id=conn.id)", sep = "")
    list(res = eval(parse(text = command)), conn.id = conn.id)
}

## ------------------------------------------------------------------------

.db.getQuery <- function (query, conn.id = 1)
{
    command <- paste(".db.getQuery.", .localVars$db[[conn.id]]$conn.pkg,
                     "(query=query, conn.id=conn.id)", sep = "")
    eval(parse(text = command))
}

## ------------------------------------------------------------------------

.db.listTables <- function (conn.id = 1)
{
    command <- paste(".db.listTables.", .localVars$db[[conn.id]]$conn.pkg,
                     "(conn.id=conn.id)", sep = "")
    eval(parse(text = command))
}

## ------------------------------------------------------------------------

.db.existsTable <- function (table, conn.id = 1)
{
    command <- paste(".db.existsTable.", .localVars$db[[conn.id]]$conn.pkg,
                     "(table=table, conn.id=conn.id)", sep = "")
    eval(parse(text = command))
}

## ------------------------------------------------------------------------

.db.listFields <- function (table, conn.id = 1)
{
    command <- paste(".db.listFields.", .localVars$db[[conn.id]]$conn.pkg,
                     "(table=table, conn.id=conn.id)", sep = "")
    eval(parse(text = command))
}

## ------------------------------------------------------------------------

.db.writeTable <- function (table, r.obj, row.names = TRUE, 
                            overwrite = FALSE, append = FALSE,
                            distributed.by = NULL, # only for GPDB
                            conn.id = 1, header, nrows = 50, sep = ",",
                            eol="\n", skip = 0, quote = '"', ...)
{
    command <- paste(".db.writeTable.", .localVars$db[[conn.id]]$conn.pkg,
                     "(table=table, r.obj=r.obj, row.names=row.names,
                      overwrite=overwrite, append=append,
                      distributed.by=distributed.by, conn.id=conn.id,
                      header=header, nrows=nrows, sep=sep, eol=eol,
                      skip=skip, quote=quote, ...)",
                     sep = "")
    eval(parse(text = command))
}

## ------------------------------------------------------------------------

.db.readTable <- function (table, rown.names = "row.names", conn.id = 1)
{       
    command <- paste(".db.readTable.", .localVars$db[[conn.id]]$conn.pkg,
                     "(table=table, row.names=row.names, conn.id=conn.id)",
                     sep = "")
    eval(parse(text = command))
}

## ------------------------------------------------------------------------

.db.removeTable <- function(table, conn.id = 1)
{
    command <- paste(".db.removeTable.", .localVars$db[[conn.id]]$conn.pkg,
                     "(table=table, conn.id=conn.id)", sep = "")
    eval(parse(text = command))
}

