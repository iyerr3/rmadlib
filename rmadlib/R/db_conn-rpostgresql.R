### Not to be called by the users, so all functions start with a prefix of dot
### and thus are not exported.

### Database related utilities, using RPostgreSQL to connect

## ------------------------------------------------------------------------
.db.connect.rpostgresql <- function(host, user, dbname, password, port, madlib)
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
        conn.pkg = "rpostgresql",
        madlib = madlib # madlib schema name
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

.db.buildTableDefinition <- function(dbObj, name, obj, field.types,
                                     row.names, dist.str, is.temp, ...)
{
    if(!is.data.frame(obj))
        obj <- as.data.frame(obj)
    if(!is.null(row.names) && row.names){
        obj  <- cbind(row.names(obj), obj)  ## can't use row.names= here
        names(obj)[1] <- "row.names"
    }
    if(is.null(field.types)){
        ## the following mapping should be coming from some kind of table
        ## also, need to use converter functions (for dates, etc.)
        field.types <- sapply(obj, RPostgreSQL::dbDataType, dbObj = dbObj)
    }
    i <- match("row.names", names(field.types), nomatch=0)
    ## if (i > 0) ## did we add a row.names value?  If so, it's a text field.
        ## field.types[i] <- RPostgreSQL::dbDataType(dbObj, field.types[["row.names"]])

    ## need to create a new (empty) table
    flds <- paste(RPostgreSQL::postgresqlQuoteId(names(field.types)), field.types)
    if (is.temp) tmp.str <- "temp"
    else tmp.str <- ""
    paste("CREATE", tmp.str, "TABLE", RPostgreSQL::postgresqlTableRef(name), "\n(",
          paste(flds, collapse=",\n\t"), "\n)", dist.str)
}


## ------------------------------------------------------------------------

.db.writeTable.rpostgresql <- function (table, r.obj, row.names, 
                                        overwrite, append, distributed.by,
                                        is.temp,
                                        idx, header, nrows = 50, sep = ",",
                                        eol="\n", skip = 0, quote = '"',
                                        field.types, ...)
{
    conn <- .localVars$db[[idx]]$conn
    conn.id <- .localVars$db[[idx]]$conn.id
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
            
            if (is.temp) {
                check.temp <- .db.existsTempTable(name, conn.id)
                name <- check.temp[[2]]
            }
            if((!is.temp && RPostgreSQL::dbExistsTable(conn,name)) ||
               (is.temp && check.temp[[1]]))
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
                if (skip>0) readLines(f, n=skip)
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
                sql <- .db.buildTableDefinition(new.con, table, d, field.types, row.names, dist.str, is.temp)
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
        else # create table from a data frame --------------------------------
        {
            if (is.temp) check.temp <- .db.existsTempTable(table, conn.id)
            if((!is.temp && RPostgreSQL::dbExistsTable(conn, name)) ||
               (is.temp && check.temp[[1]]))
            {
                if (overwrite) {
                    if (!RPostgreSQL::dbRemoveTable(conn, name))
                    {
                        warning(paste("table", name, "couldn't be overwritten"))
                        return(FALSE)
                    }
                }
                else if (!append)
                {
                    warning(paste("table", name, "exists in database: aborting assignTable"))
                    return(FALSE)
                }
            }
            else
            {
                if (missing(field.types)) field.types <- NULL
                sql <- .db.buildTableDefinition(conn, table, r.obj, field.types, row.names, dist.str, is.temp)
                rs <- try(RPostgreSQL::dbSendQuery(conn, sql))
                if (is.temp) name <- (.db.existsTempTable(table, conn.id))[[2]]
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
                              name = name, value = value, row.names = row.names,
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
