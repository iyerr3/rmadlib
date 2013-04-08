## test madlib.lm

library(rmadlib)

db.connect(dbname = "qianh1", user = "qianh1", host = "localhost", port = 5433)

db.list()

x <- db.data.frame("madlibtestdata.lin_ornstein")

names(x)

dim(x)

fit <- madlib.lm(interlocks ~ .-1-nation| nation, data = x, heteroskedasticity = T)

fit

fit <- madlib.lm(interlocks ~ assets + sector + nation + I(assets^2), data = x, heteroskedasticity = T)

fit

## ------------------------------------------------------------------------

git <- madlib.glm(I(interlocks<10) ~ assets + sector + nation, data = x, family = "binomial")

git


dat <- rmadlib:::.db.getQuery("select * from madlibtestdata.lin_ornstein")

dat$y <- factor(as.integer(dat$interlocks < 10) + 2)

g <- glm(y ~ assets + sector + nation, family = binomial, data = dat)

summary(g)
