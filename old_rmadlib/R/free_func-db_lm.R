
## ------------------------------------------------------------------------
## Free function
##
## Linear regression on data in db
## The indb object can be either a created or uncreated data object
## ------------------------------------------------------------------------


## no finished yet

## lm function
## can hard-override stats::lm

db.lm <- function(formula, data)
{
    ## create a fake data.frame only to extract
    ## terms when there is "." in formula
    fake.data <- data.frame(t(colnames(data)))
    colnames(fake.data) <- colnames(data)
    f.terms <- terms(formula, data = fake.data) # formula terms
    f.factors <- attr(f.terms, "factors") # the 1st is the dependent variable
    f.labels <- attr(f.terms, "term.labels") # each terms on the right side
    f.intercept <- attr(f.terms, "intercept")
    labels <- gsub(":", "*", f.labels) # replace interaction : with *
    labels <- gsub("I\\((.*)\\)", "\\1", labels) # remove I()
    ##
    dep.var <- rownames(f.factors)[1] # dependent variable
    ## with or without intercept
    if f.intercept == 0:
        intercept.str <- ""
    else
        intercept.str <- "1,"
    ind.var <- paste("array[", intercept.str,
                     paste(labels, collapse = ","),
                     "]", sep = "") # independent variable
    ## should return an object with class lm.db
    ## so that predict.lm.db can be defined and used

    ## temporary result table
    tbl_output <- .unique.string() # a random string
    ## run select madlib.logregr_train('obj.name(data)', 'tbl_output', 'dep.var', 'ind.var')
    ## extract result from tbl_output

    ## TO BE FINISHED
    
}

.hard.overridden.lm <- function(formula, data, subset, weights, na.action,
                                method = "qr", model = TRUE, x = FALSE, y = FALSE, qr = TRUE,
                                singular.ok = TRUE, contrasts = NULL, offset, ...)
{
    if (is(data, "db.data.frame")) # also need to check data in fomula if no data
    {
        db.lm(...)
    }
    else
    {
        .origin.stats.lm(...)
    }
}
