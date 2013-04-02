### Not to be called by the users, so all functions start with a prefix of dot
### and thus are not exported.

### Database related utilities, using RPostgreSQL to connect

## ------------------------------------------------------------------------
.db.connect.rpostgresql <- function(host, user, dbname, password)
{
    if (is.null(.localVars$drv$rpostgresql))
        .localVars$drv$rpostgresql <- DBI::dbDriver("PostgreSQL")
    
    n.db <- length(.localVars$db)
    db.connection <- RPostgreSQL::dbConnect(.localVars$drv$rpostgresql, host=host, user=user, dbname=dbname, password=password)
    .localVars$db[[n.db+1]] <- list(
                               conn = db.connection,
                               host = host,
                               user = user,
                               dbname = dbname, # database name
                               conn.pkg = "rpostgresql" # which R package is used to connected to database
                               )
    .localVars$conn.type[["rpostgresql"]] <- c(.localVars$conn.type[["rpostgresql"]], n.db+1)
    return(n.db + 1) # return the connection ID
}

## ------------------------------------------------------------------------

.db.unloadDriver.rpostgresql <- function(drv)
{
    RPostgreSQL::dbUnloadDriver(drv)
}

## ------------------------------------------------------------------------

.db.disconnect.rpostgresql <- function(conn.id)
{
    RPostgreSQL::dbDisconnect (.localVars$db[[conn.id]]$conn)
}

## ------------------------------------------------------------------------

.db.sendQuery.rpostgresql <- function(query, conn.id)
{
    RPostgreSQL::dbSendQuery(.localVars$db[[conn.id]]$conn, query)
}

## ------------------------------------------------------------------------

.db.getQuery.rpostgresql <- function(query, conn.id)
{
    RPostgreSQL::dbGetQuery(.localVars$db[[conn.id]]$conn, query)
}

## ------------------------------------------------------------------------

.db.listTables.rpostgresql <- function(conn.id)
{
    RPostgreSQL::dbListTables(.localVars$db[[conn.id]]$conn)
}

## ------------------------------------------------------------------------

.db.existsTable.rpostgresql <- function(table, conn.id)
{
    RPostgreSQL::dbExistsTable(.localVars$db[[conn.id]]$conn, table)
}

## ------------------------------------------------------------------------

.db.listFields.rpostgresql <- function(table, conn.id)
{
    RPostgreSQL::dbListFields(.localVars$db[[conn.id]]$conn, table)
}

## ------------------------------------------------------------------------

.db.writeTable.rpostgresql <- function (table, r.obj, row.names, 
                                        overwrite, append, distributed.by,
                                        conn.id, header, nrows = 50, sep = ",",
                                        eol="\n", skip = 0, quote = '"', ...)
{
    conn <- .localVars$db[[conn.id]]
    name <- table
    value <- r.obj
    ## only for GPDB
    ## This why this function is so complicated
    if (is.null(distributed.by)) {
        dist.str <- ""
    } else {
        if (is.na(distributed.by)) # NA means distributed randomly
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
            fn <- file.path(dirname(value), basename(value))
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
                    RPostgreSQL::postgresqlBuildTableDefinition(new.con, name, obj=d, field.types = field.types,
                                                                row.names = row.names)
                sql <- paste(sql, dist.str)
                rs <- try(RPostgreSQL::dbSendQuery(new.con, sql))
                if(inherits(rs, RPostgreSQL::ErrorClass)){
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

            if(missing(field.types) || is.null(field.types))
            {
                ## the following mapping should be coming from some kind of table
                ## also, need to use converter functions (for dates, etc.)
                field.types <- sapply(r.obj, RPostgreSQL::dbDataType, dbObj = conn)
            }

            i <- match("row.names", names(field.types), nomatch=0)
            if(i>0) ## did we add a row.names value?  If so, it's a text field.
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
                sql <- paste(sql1, sql2, sql3, dist.str, sep="")
                rs <- try(RPostgreSQL::dbSendQuery(new.con, sql))
                if(inherits(rs, RPostgreSQL::ErrorClass))
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

        ## After the table has been created, one can append data to it
        RPostgreSQL::dbWriteTable(conn = .localVars$db[[conn.id]]$conn,
                                  name = table, value = r.obj, row.names = row.names,
                                  overwrite = overwrite, append = TRUE)
    }
    else
    {
        RPostgreSQL::dbWriteTable(conn = .localVars$db[[conn.id]]$conn,
                                  name = table, value = r.obj,
                                  row.names = row.names, overwrite = overwrite,
                                  append = append)
    }
}

## ------------------------------------------------------------------------

.db.readTable.rpostgresql <- function (table, row.names, conn.id)
{
    RPostgreSQL::dbReadTable(conn = .localVars$db[[conn.id]]$conn,
                             name = table, row.names = row.names)
}

## ------------------------------------------------------------------------

.db.removeTable.rpostgressql <- function (table, conn.id)
{
    RPostgreSQL::dbRemoveTable(conn = .localVars$db[[conn.id]]$conn,
                               name = table)
}
