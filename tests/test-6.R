
mean.dbdf <- function (x) { 23 }

x <- 10
class(x) <- "dbdf"

mean(x)

setGeneric("colMeans", signature = "x")

setMethod("colMeans", signature(x = "dbdf"),
          function (x) {31})

colMeans.dbdf <- function (x, na.rm = FALSE, dims = 1, ...) {31}

colMeans(x)

colMeans.dbdf(x)

sum.dbdf <- function(x, na.rm = FALSE) {37}

sum(x)

sum.dbdf(x)

mean.dbdf(x)

## ------------------------------------------------------------------------

mc <- function (formula, data, subset, weights, na.action, method = "qr",
                model = TRUE, x = FALSE, y = FALSE, qr = TRUE,
                singular.ok = TRUE, contrasts = NULL, offset, ...)
{
    ret.x <- x
    ret.y <- y
    cl <- match.call()
    mf <- match.call(expand.dots = FALSE)
    m <- match(c("formula", "data", "subset", "weights", "na.action",
                 "offset"), names(mf), 0L)
    mf <- mf[c(1L, m)]
    mf$drop.unused.levels <- TRUE
    mf[[1L]] <- as.name("model.frame")
    mf <- eval(mf, parent.frame())
    list(mf = mf, m = m)
}

z <- mc(y ~ ., data.frame(a = 1:3, y = 2:4))

z

mt <- attr(z$mf, "terms")

model.response(z$mf, "numeric")

model.matrix(mt, z$mf, contrasts)

## ------------------------------------------------------------------------

x <- data.frame(y = 1:3, a = 1:3, b = 2:4)

v <- 3
class(v) <- c("dbdf", "data.frame")
names.dbdf <- function(x) { c("y", "x1", "x2")}
length.dbdf <- function(x) {3}

attr(v, "names") <- c("y", "x1", "x2")

names(v)

terms(y ~ ., data = x)

terms(y ~ ., data = v)

## ------------------------------------------------------------------------

setGeneric("summary", signature = "object")

summary.dbdf1 <- function (x) {23}

setMethod("summary", signature("dbdf"),
          function (object) {57})

summary(x)

y <- 1
class(y) <- "dbdf1"

summary(y)
