### Not to be called by the users, so all functions start with a prefix of dot
### and thus are not exported.

### Database related utilities, using RPostgreSQL to connect

## ------------------------------------------------------------------------
.db.connect.rpostgresql <- function(host, user, dbname, password, port)
{
    if (is.null(.localVars$drv$rpostgresql))
        .localVars$drv$rpostgresql <- DBI::dbDriver("PostgreSQL")
    
    n.db <- length(.localVars$db)
    db.connection <- RPostgreSQL::dbConnect(.localVars$drv$rpostgresql,
                                            host=host, user=user,
                                            dbname=dbname,
                                            password=password,
                                            port = port)
    if (length(.localVars$conn.id) == 0)
        conn.id <- 1
    else
        conn.id <- max(.localVars$conn.id[,2]) + 1
    
    .localVars$db[[n.db+1]] <- list(
        conn = db.connection,
        conn.id = conn.id,
        host = host,
        user = user,
        dbname = dbname, # database name
        # which R package is used to connected to database
        conn.pkg = "rpostgresql" 
        )

    .localVars$conn.id <- rbind(.localVars$conn.id,
                                   c(conn.id, n.db + 1))
    
    .localVars$conn.type[["rpostgresql"]] <- c(
        .localVars$conn.type[["rpostgresql"]],
        conn.id)
    return(conn.id) # return the connection ID
}

## ------------------------------------------------------------------------

.db.unloadDriver.rpostgresql <- function(drv)
{
    RPostgreSQL::dbUnloadDriver(drv)
}

## ------------------------------------------------------------------------

.db.disconnect.rpostgresql <- function(idx)
{
    RPostgreSQL::dbDisconnect (.localVars$db[[idx]]$conn)
}

## ------------------------------------------------------------------------

.db.sendQuery.rpostgresql <- function(query, idx)
{
    RPostgreSQL::dbSendQuery(.localVars$db[[idx]]$conn, query)
}

## ------------------------------------------------------------------------

.db.fetch.rpostgresql <- function(res, n)
{
    RPostgreSQL::fetch(res, n)
}

## ------------------------------------------------------------------------

.db.getQuery.rpostgresql <- function(query, idx)
{
    RPostgreSQL::dbGetQuery(.localVars$db[[idx]]$conn, query)
}

## ------------------------------------------------------------------------

.db.listTables.rpostgresql <- function(idx)
{
    RPostgreSQL::dbListTables(.localVars$db[[idx]]$conn)
}

## ------------------------------------------------------------------------

.db.existsTable.rpostgresql <- function(table, idx)
{
    RPostgreSQL::dbExistsTable(.localVars$db[[idx]]$conn, table)
}

## ------------------------------------------------------------------------

.db.listFields.rpostgresql <- function(table, idx)
{
    RPostgreSQL::dbListFields(.localVars$db[[idx]]$conn, table)
}

## ------------------------------------------------------------------------

.db.writeTable.rpostgresql <- function (table, r.obj, row.names, 
                                        overwrite, append, distributed.by,
                                        idx, header, nrows = 50, sep = ",",
                                        eol="\n", skip = 0, quote = '"',
                                        field.types, ...)
{
    conn <- .localVars$db[[idx]]$conn
    name <- table
    value <- r.obj
    ## only for GPDB
    ## This why this function is so complicated
    if (is.null(distributed.by)) {
        dist.str <- ""
    } else {
        if (!.is.arg.string(distributed.by))
            stop("distributed.by must be a string or NULL!")
        if (distributed.by == "") # "" means distributed randomly
            dist.str <- "DISTRIBUTED RANDOMLY"
        else
            dist.str <- paste("DISTRIBUTED BY (", distributed.by, ")", sep = "")
    }
    
    if (!append)
    {
        ## need to create the table first
        if (is.character(r.obj)) # create from file
        {
            new.con <- conn
            
            if(RPostgreSQL::dbExistsTable(conn,name))
            {
                if(overwrite)
                {
                    if(!RPostgreSQL::dbRemoveTable(conn, name))
                    {
                        warning(paste("table", name, "couldn't be overwritten"))
                        return(FALSE)
                    }
                }
                else if(!append)
                {
                    warning(paste("table", name, "exists in database: aborting dbWriteTable"))
                    return(FALSE)
                }
            }
            
            ## compute full path name (have R expand ~, etc)
            fn <- file.path(dirname(r.obj), basename(r.obj))
            if(missing(header) || missing(row.names))
            {
                f <- file(fn, open="r")
                if(skip>0)
                    readLines(f, n=skip)
                txtcon <- textConnection(readLines(f, n=2))
                flds <- count.fields(txtcon, sep)
                close(txtcon)
                close(f)
                nf <- length(unique(flds))
            }
            
            if(missing(header)) header <- nf==2
            
            if(missing(row.names))
            {
                if(header)
                    row.names <- if(nf==2) TRUE else FALSE
                else
                    row.names <- FALSE
            }
            
            new.table <- !RPostgreSQL::dbExistsTable(conn, name)
            if(new.table)
            {
                ## need to init table, say, with the first nrows lines
                d <- read.table(fn, sep=sep, header=header, skip=skip, nrows=nrows, ...)
                sql <-
                    RPostgreSQL::postgresqlBuildTableDefinition(new.con, name, obj=d,
                                                                field.types = field.types,
                                                                row.names = row.names)
                sql <- paste(sql, dist.str)
                rs <- try(RPostgreSQL::dbSendQuery(new.con, sql))
                if(inherits(rs, RPostgreSQL:::ErrorClass)){
                    warning("could not create table: aborting postgresqlImportFile")
                    return(FALSE)
                }
                else
                    RPostgreSQL::dbClearResult(rs)
            }
            else if(!append)
            {
                warning(sprintf("table %s already exists -- use append=TRUE?", name))
            }
        }
        else # create table from a data frame 
        {
            if(!is.data.frame(r.obj))
                r.obj <- as.data.frame(r.obj)

            if(row.names)
            {
                r.obj <- cbind(row.names(r.obj), r.obj)  ## can't use row.names= here
                names(r.obj)[1] <- "row.names"
            }

            if(missing(field.types) || is.null(field.types)) {
                ## the following mapping should be coming from some kind of table
                ## also, need to use converter functions (for dates, etc.)
                field.types <- sapply(r.obj, RPostgreSQL::dbDataType, dbObj = conn)
            }

            i <- match("row.names", names(field.types), nomatch=0)
            if (i>0) ## did we add a row.names r.obj?  If so, it's a text field.
                ## MODIFIED -- Sameer
                field.types[i] <- RPostgreSQL::dbDataType(dbObj=conn, field.types[row.names])
            
            new.con <- conn
            
            if(RPostgreSQL::dbExistsTable(conn, name))
            {
                if(overwrite){
                    if(!RPostgreSQL::dbRemoveTable(conn, name))
                    {
                        warning(paste("table", name, "couldn't be overwritten"))
                        return(FALSE)
                    }
                }
                else if(!append)
                {
                    warning(paste("table", name, "exists in database: aborting assignTable"))
                    return(FALSE)
                }
            }
            else
            {
                ## need to re-test table for existance
                ## need to create a new (empty) table
                sql1 <- paste("create table ", RPostgreSQL::postgresqlTableRef(name), "\n(\n\t", sep="")
                sql2 <- paste(paste(RPostgreSQL::postgresqlQuoteId(names(field.types)), field.types), collapse=",\n\t",
                              sep="")
                sql3 <- "\n)\n"
                ## sql <- paste(sql1, sql2, sql3, dist.str, sep="")
                sql <- paste(sql1, sql2, sql3, sep="")
                rs <- try(RPostgreSQL::dbSendQuery(new.con, sql))
                if(inherits(rs, RPostgreSQL:::ErrorClass))
                {
                    warning("could not create table: aborting assignTable")
                    return(FALSE)
                }
                else
                {
                    RPostgreSQL::dbClearResult(rs)
                }
            }
        }
    }

    ## After the table has been created, one can append data to it
    RPostgreSQL::dbWriteTable(conn = .localVars$db[[idx]]$conn,
                              name = table, value = value, row.names = row.names,
                              overwrite = overwrite, append = TRUE,
                              header = header, nrows = nrows, sep = sep,
                              eol=eol, skip = skip, quote = quote,
                              field.types = field.types, ...)
}

## ------------------------------------------------------------------------

.db.readTable.rpostgresql <- function (table, row.names, idx)
{
    RPostgreSQL::dbReadTable(conn = .localVars$db[[idx]]$conn,
                             name = table, row.names = row.names)
}

## ------------------------------------------------------------------------

.db.removeTable.rpostgresql <- function (table, idx)
{
    RPostgreSQL::dbRemoveTable(conn = .localVars$db[[idx]]$conn,
                               name = table)
}
