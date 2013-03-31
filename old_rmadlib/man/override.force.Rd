\name{override.force}
\alias{override.force}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
override.force(choice = "all")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{choice}{
%%     ~~Describe \code{choice} here~~
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
function (choice = "all") 
{
    print(paste("The following functions will be overridden to gain the ability to deal with", 
        .db.data.class[1]), quote = FALSE)
    print(paste("However, when dealing with ", .db.data.class[1], 
        ", not all arguments will be utilized.", sep = ""), quote = FALSE)
    if (choice == "all") {
        for (i in seq(along = .localConst.hard.override.funcs)) .override.one(.localConst.hard.override.funcs[i])
    }
    else {
        for (i in seq(along = choice)) .override.one(choice[i])
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
