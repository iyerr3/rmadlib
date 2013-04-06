setGeneric ("sample", signature = "x")

setMethod(
    "sample",
    signature(x = "data.frame"),
    function (x, size, replace = FALSE) {
        stop("need a definition for the method here")
    })

sample(data.frame(x=1:3), 2)
