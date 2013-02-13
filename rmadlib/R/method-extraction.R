
### method for extracting parts of database object

setMethod("$",
          signature(x = "indb.created"),
          function (x, name)
          {
              if (! name %in% names(x))
                  stop(paste("Column", name, "does not exist!"))

              data.frame.indb.uncreated(x = paste("select", name, "from", tbl(x)), con.id = con.id(x))
          })

setMethod("[[",
    signature(x = "indb.created"),
    function (x, i, ...) 
    {
        na <- nargs()
        if (na == 1)
            stop("What do you want to do?")
        
        if (na == 2)
        {
            if (is.character(i))
                if (i %in% names(x)) {
                    return (data.frame.indb.uncreated(x = paste("select", i, "from", tbl(x)), con.id = con.id(x)))
                } else {
                    stop(paste("Column", i, "does not exist!"))
                }
            else
            {
                ii <- as.integer(i)
                if (ii < 1 || ii > length(names(x)))
                    stop("No such column!")
                return (data.frame.indb.uncreated(x = paste("select", names(x)[ii], "from", tbl(x)), con.id = con.id(x)))
            }
        }
        else if (na == 3)
        {
            if (length(id.col(x)) == 0)
                stop("There is no unique ID associated with each row of the table!")

            j <- ..1
            if (is.character(j))
                if (j %in% names(x)) {
                    col.name <- j
                } else {
                    stop(paste("Column", i, "does not exist!"))
                }
            else
            {
                jj <- as.integer(j)
                if (jj < 1 || jj > length(names(x)))
                    stop("No such column!")
                col.name <- names(x)[jj]
            }
            return (data.frame.indb.uncreated(x = paste("select", col.name, "from", tbl(x), "where", id.col(x), "=", i), con.id = con.id(x)))
        }
    }
)
