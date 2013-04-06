
## ------------------------------------------------------------------------
## utility functions exposed to the users
## ------------------------------------------------------------------------

is.db.data.frame <- function (x)
{
    inherits(x, "db.data.frame")
}

## ------------------------------------------------------------------------

## Grab a portion of the data
portion <- function(x, nrows = 100)
{
    if (! inherits(x, "db.data.frame"))
        stop(paste(deparse(substitute(x)), "must be a db.data.frame object!"))

    if (! inherits(x, "db.view")) {
        cat(deparse(substitute(x)),
            "points to a view in the database",
            dbname(conn.id(x)),
            "and it takes time to evaluate and extract portion of it !\n")
        go <- .read.input("Do you really want to continue ? (Yes/No) : ",
                          c("yes", "y", "no", "n"))
        if (go == "no" || go == "n") return
    }

    .db.getQuery(paste("select * from", content(x), "limit", nrows), conn.id(x))
}
