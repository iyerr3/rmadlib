\name{print.lm.madlib}
\alias{print.lm.madlib}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
print.lm.madlib(x, digits = max(3L, getOption("digits") - 3L), ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{digits}{
%%     ~~Describe \code{digits} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x, digits = max(3L, getOption("digits") - 3L), ...) 
{
    if (x$has.intercept) 
        rows <- c("(Intercept)", x$ind.vars)
    else rows <- x$ind.vars
    ind.width <- .max.width(rows)
    cat("\nLinear Regression Result\n")
    cat("\nCall:\n", x$call, "\n", sep = "")
    for (i in seq_len(x$grps)) {
        cat("\n---------------------------------------\n\n")
        if (!is.null(x$grp.cols)) {
            cat("When\n")
            for (col in x$grp.cols) cat(col, ": ", x[[col]][i], 
                "\n", sep = "")
            cat("We have\n\n")
        }
        cat("Coefficients:\n")
        coef <- format(x$coef, digits = digits)
        std.err <- format(x$std_err, digits = digits)
        t.stats <- format(x$t_stats, digits = digits)
        stars <- rep("", length(x$p_values))
        for (j in seq_len(length(x$p_values))) if (x$p_values[j] < 
            0.001) 
            stars[j] <- "***"
        else if (x$p_values[j] < 0.01) 
            stars[j] <- "**"
        else if (x$p_values[j] < 0.05) 
            stars[j] <- "*"
        else if (x$p_values[j] < 0.1) 
            stars[j] <- "."
        else stars[j] <- " "
        p.values <- paste(format(x$p_values, digits = digits), 
            stars)
        output <- data.frame(cbind(Estimate = coef, `Std. Error` = std.err, 
            `t value` = t.stats, `Pr(>|t|)` = p.values), row.names = rows)
        print(output)
        cat("---\n")
        cat("Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1\n\n")
        cat("R-squared:", x$r2, "\n")
        cat("Condition Number:", x$condition_no, "\n")
        if (!is.null(x$bp_stats)) {
            cat("Breusch\033$(G!9\033(BPagan test statistics:", 
                x$bp_stats, "\n")
            cat("Breusch\033$(G!9\033(BPagan test p-value:", 
                x$bp_p_value, "\n")
        }
    }
    cat("\n")
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
