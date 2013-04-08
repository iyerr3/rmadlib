
## ------------------------------------------------------------------------
## extraction function $ and [[
## ------------------------------------------------------------------------

setMethod (
    "$",
    signature(x = "db.data.frame"),
    function (x, name) {
        if (! name %in% names(x))
            stop(paste("Column", name, "does not exist!"))

        if (identical(x@.id.col, character(0)))
            id.str <- ""
        else
            id.str <- paste(x@.id.col, ", ", sep = "")
        
        new("db.Rquery",
            .content = paste("select ", id.str, name,
            " from ", content(x), sep = ""),
            .conn.id = conn.id(x),
            .col.name = name,
            .id.col = x@.id.col)
    },
    valueClass = "db.Rquery")

## ------------------------------------------------------------------------

setMethod (
    "$",
    signature(x = "db.Rquery"),
    function (x, name) {
        if (! name %in% x@.col.name)
            stop(paste("Column", name, "does not exist!"))

        if (identical(x@.id.col, character(0)))
            id.str <- ""
        else
            id.str <- paste(x@.id.col, ", ", sep = "")

        new("db.Rquery",
            .content = paste("select ", id.str, name,
            " from (", content(x), ") s", sep = ""),
            .conn.id = conn.id(x),
            .col.name = name,
            .id.col = x@.id.col)
    },
    valueClass = "db.Rquery"
    )

## ------------------------------------------------------------------------

setMethod(
    "[[",
    signature(x = "db.data.frame"),
    function (x, i, j, ...) {
        na <- nargs()
        if (na == 1)
            stop("What do you want to do?")

        if (identical(x@.id.col, character(0)))
            id.str <- ""
        else
            id.str <- paste(x@.id.col, ", ", sep = "")
        
        if (na == 2)
        {
            if (is.character(i))
                if (i %in% names(x)) {
                    new("db.Rquery",
                        .content = paste("select ", id.str,
                        i, " from ", content(x), sep = ""),
                        .conn.id = conn.id(x),
                        .col.name = i,
                        .id.col = x@.id.col)
                } else {
                    stop(paste("Column", i, "does not exist!"))
                }
            else
            {
                ii <- as.integer(i)
                if (ii < 1 || ii > length(names(x)))
                    stop("No such column!")

                new("db.Rquery",
                    .content = paste("select ", id.str,
                    names(x)[ii], " from ", content(x),
                    sep = ""),
                    .conn.id = conn.id(x),
                    .col.name = names(x)[[ii]],
                    .id.col = x@.id.col)
            }
        }
        else if (na == 3)
        {
            
            if (identical(x@.id.col, character(0)) == 0)
                stop("There is no unique ID associated with each row of the table!")

            if (is.character(j))
                if (j %in% names(x)) {
                    col.name <- j
                } else {
                    stop(paste("Column", j, "does not exist!"))
                }
            else
            {
                jj <- as.integer(j)
                if (jj < 1 || jj > length(names(x)))
                    stop("No such column!")
                col.name <- names(x)[jj]
            }

            new("db.Rquery",
                .content = paste("select ", id.str, col.name,
                " from ", content(x), " where ", x@.id.col,
                " = ", i, sep = ""),
                .col.name = col.name,
                .conn.id = conn.id(x),
                .id.col = x@.id.col)
        }
    },
    valueClass = "db.Rquery")

## ------------------------------------------------------------------------

setMethod(
    "[[",
    signature(x = "db.Rquery"),
    function (x, i, j, ...) {
        stop("To be implemented!")
    },
    valueClass = "db.Rquery")
