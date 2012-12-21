
## define function to get dimensions of a data table

madlib.dim <- function(x)
{
    attrs <- attributes(x)
    if (is.null(attrs$inDatabase) || !attrs$inDatabase)
        return (.origin.base.dim(x))
    else
        
}
