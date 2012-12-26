## Not to be called by the users, so all functions start with a prefix of dot
## and thus are not exported.

## Check arguments of function to make sure that they are legal

## ----------------------------------------------------------------
.is.arg.string <- function(arg)
{
    is.character(arg)
}

.is.con.id.valid <- function(con.id)
{
    !is.na(con.id) && con.id >= 1 && con.id <= length(.localVars$db)
}
