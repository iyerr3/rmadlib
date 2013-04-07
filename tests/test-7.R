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
