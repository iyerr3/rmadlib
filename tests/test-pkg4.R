install.packages("rmadlib_0.0.1.tar.gz", repos = NULL, type = "source")
library(rmadlib)
db.connect(host="localhost", user="qianh1", dbname="qianh1", port=5433)

x <- data.frame(a = 1:3, b = 2:4)

rmadlib:::.db.removeTable("testx1")

rmadlib:::.db.writeTable(table = "testx1", r.obj = x, distributed.by = NULL, row.names = TRUE, is.temp = TRUE)

res <- rmadlib:::.db.readTable("testx1")

res

t <- try(rmadlib:::.db.writeTable(table = "testx1", r.obj = x, distributed.by = NULL, row.names = TRUE, is.temp = TRUE))


y <- db.data.frame("testx1")

dim(y)

y@.col.data_type

y@.table.type

z <- as.db.data.frame(x, "testx8", add.row.names = TRUE, id.col = character(0), is.temp = FALSE)

z@.col.data_type

z@.table.type

w <- as.db.data.frame(x = "/Users/qianh1/workspace/rwrapper/input_data.csv", table.name = "testx8", conn.id = 1, add.row.names = FALSE, id.col = character(0), is.temp = FALSE, header = FALSE, skip=2, col.names = c(paste("x", 1:8, sep=""), "y"))

w@.col.data_type

w@.table.type


## ------------------------------------------------------------------------

analyze.formula <- function (formula, data)
{
    f.str <- strsplit(deparse(formula), "\\|")[[1]]
    f1 <- formula(f.str[1]) # formula
    f2 <- f.str[2] # grouping columns, might be NA
    if (!is.na(f2)) {
        f2.terms <- terms(formula(paste("~", f2)))
        f2.labels <- attr(f2.terms, "term.labels")
        inter <- intersect(f2.labels, names(data))
        if (length(inter) != length(f2.labels))
            stop("The grouping part of the formula is not quite right!")
        grp <- paste(f2.labels, collapse = " ")
    } else {
        f2.labels <- NULL
        grp <- NULL
    }
    ## create a fake data.frame only to extract
    ## terms when there is "." in formula
    fake.data <- data.frame(t(names(data)))
    colnames(fake.data) <- names(data)
    f.terms <- terms(f1, data = fake.data) # formula terms
    f.factors <- attr(f.terms, "factors") # the 1st row is the dependent variable
    f.labels <- attr(f.terms, "term.labels") # each terms on the right side
    f.intercept <- attr(f.terms, "intercept")
    labels <- gsub(":", "*", f.labels) # replace interaction : with *
    labels <- gsub("I\\((.*)\\)", "\\1", labels) # remove I()
    if (!is.null(f2.labels)) # remove grouping columns
        labels <- setdiff(labels, f2.labels)
    ##
    dep.var <- rownames(f.factors)[1] # dependent variable
    ## with or without intercept
    if (f.intercept == 0)
        intercept.str <- ""
    else
        intercept.str <- "1,"
    ind.var <- paste("array[", intercept.str,
                     paste(labels, collapse = ","),
                     "]", sep = "") # independent variable
    ##
    list(dep.str = dep.var, ind.str = ind.var, grp.str = grp)
}

dat <- data.frame(x1 = 1:3, x2 = 1:3, x3 = 1:3, x4 = 1:3, y = 1:3)

analyze.formula(formula = log(y) ~ x1 * (x1 + x2) + I(x2^2) + exp(1+x1) | x3 + x4, data = dat)

rmadlib:::.db.str2vec("x3 x4", type = "character")
