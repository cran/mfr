\name{timings}
\alias{timings}
\alias{fit.timings}
\title{
Timings of Singular.
}
\description{
Run some timings of Singular.
}
\usage{
timings(N = 100, ns = 10:20, ps = 0.1)
fit.timings(z,from=10,by.max=TRUE)
}
\arguments{
  \item{N}{
number of runs.
}
  \item{ns}{
orders of the graphs to generate.
}
  \item{ps}{
probabilities passed to \code{erdos.renyi.game}.
}
  \item{z}{
the output from \code{timings}.
}
  \item{from,by.max}{
an exponential model is fit from \code{size(g)=from} on, fitted to the maximum
time if \code{by.max} is TRUE, otherwise fit to all the times.
}
}
\details{
This runs a bunch of Singular sessions and for a bunch of graphs and
times the sessions. \code{fit.timings} fits an exponential to the
times and plots the results.
}
\value{
\code{timings} returns a list containing a list of timings and the sizes.
\code{fit.timings} returns a linear model (via \code{lm}).
}
\author{
David J. Marchette, dmarchette@gmail.com
}
\note{
If \code{ns} is too large, or \code{ps} is too large, or you don't have
much memory, this can either take forever or crash, so be warned. Try
it for the default values before you get carried away. You can use this
to determine how close the estimates provided by the code match the
times it actually takes on your machine.
}


\seealso{
\code{\link{singular}}, \code{\link{lm}}.
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ graphs }
