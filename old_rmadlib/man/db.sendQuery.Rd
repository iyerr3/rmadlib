\name{db.sendQuery}
\alias{db.sendQuery}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
db.sendQuery(con.id = 1, query)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{con.id}{
%%     ~~Describe \code{con.id} here~~
}
  \item{query}{
%%     ~~Describe \code{query} here~~
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
function (con.id = 1, query) 
{
    if (!.is.con.id.valid(con.id)) 
        stop("There is no such connection!")
    if (!.is.arg.string(query)) 
        stop("The query must be a string!")
    command <- paste(".db.sendQuery.", .localVars$db[[con.id]]$rcon.pkg, 
        "(con.id=", con.id, "query=", query, ")", sep = "")
    eval(parse(text = command))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
