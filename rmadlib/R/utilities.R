
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
        stop(paste(deparse(substitute(x)),
                   "must be a db.data.frame object!"))

    if (! inherits(x, "db.view")) {
        cat(deparse(substitute(x)),
            "points to a view in the database",
            dbname(conn.id(x)),
            "and it takes time to evaluate and extract portion of it !\n")
        go <- .read.input("Do you really want to continue ? (Yes/No) : ",
                          c("yes", "y", "no", "n"))
        if (go == "no" || go == "n") return
    }

    .db.getQuery(paste("select * from", content(x), "limit", nrows),
                 conn.id(x))
}

## ------------------------------------------------------------------------
## ------------------------------------------------------------------------
## Sampling function
## ------------------------------------------------------------------------
## ------------------------------------------------------------------------

setGeneric ("sample", signature = "x")

setMethod(
    "sample",
    signature(x = "db.table"),
    function (x, size, replace = FALSE, prob = NULL) {
        if (length(nchar(x@.id.col)) != 0) # has a valid id.col
        {

        }
        cat("To be implemented !\n")
        return (0)
    })

## ------------------------------------------------------------------------

setMethod(
    "sample",
    signature(x = "db.view"),
    function (x, size, replace = FALSE, prob = NULL) {
        cat("To be implemented !\n")
        return (0)
    })

## ------------------------------------------------------------------------

setMethod(
    "sample",
    signature(x = "db.Rquery"),
    function (x, size, replace = FALSE, prob = NULL) {
        cat("To be implemented !\n")
        return (0)
    })
