
## no finished yet

## lm function
## can hard-override stats::lm

rmad.lm <- function(...)
{

    ## should return an object with class lm.indb
    ## so that predict.lm.indb can be defined and used
}

.hard.overridden.lm <- function(formula, data, subset, weights, na.action,
                                method = "qr", model = TRUE, x = FALSE, y = FALSE, qr = TRUE,
                                singular.ok = TRUE, contrasts = NULL, offset, ...)
{
    if (class(data)[1] == .db.data.class[1])
    {
        rmad.lm(...)
    }
    else
    {
        .origin.stats.lm(...)
    }
}
