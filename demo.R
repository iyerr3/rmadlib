
library(rmadlib)

db.connect(dbname = "qianh1", user = "qianh1", host = "localhost", port = 5433)

db.list()

x <- db.data.frame("madlibtestdata.lin_ornstein")

names(x)

dim(x)

portion(x, 10)

## linear regression conditioned on nation value
## i.e. grouping
fit <- madlib.lm(interlocks ~ . | nation, data = x, heteroskedasticity = T)

fit

fit <- madlib.lm(interlocks ~ assets + sector + nation + I(assets^2), data = x, heteroskedasticity = T)

fit

## ------------------------------------------------------------------------

git <- madlib.glm(I(interlocks<10) ~ assets + sector + nation, data = x, family = "binomial")

git

## ------------------------------------------------------------------------

z <- x$assets

w <- x[["nation"]]

## ------------------------------------------------------------------------

dat <- read.csv("pipe_RIS_2004_2011_extract.csv", header = TRUE, sep = "|")

z <- as.db.data.frame(dat, "dcpris_randomly", add.row.names = FALSE, is.temp = FALSE, distributed.by = "")
