\name{matchRatio}
\alias{matchRatio}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Function to carry out sample matching a number of times
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Pulling a given number of times (without replacement) from the available panelists, to create \code{n}
matches for each target member
}
\usage{
matchRatio(data, metric, n, exact = c(), ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{Dataframe holding all the panelists and target members}
  \item{metric}{Name of matching metric to pass into matchit, e.g. 'mahalanobis'}
  \item{n}{Integer; number of desired matches for each target member}
  \item{exact}{List or vector; names of variables in \code{data} to match exactly}
  \item{id}{Character string; name of variable to identify individual rows in target}
  \item{treat}{Character string; name of variable to indicate target/panel membership}
  \item{panelid}{Character string; name of variable to identify panelists}
%%     ~~Describe \code{x} here~~
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
  \item{ids}{Dataframe of target and panel ids in each round of matches}
  \item{matches}{Match objects returned by matchit in each round of matching}
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
function (x)
{
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
