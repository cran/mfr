\name{estimate.mfr.time}
\alias{estimate.mfr.time}
\title{
Estimate Computation Time
}
\description{
A crude estimate of the time it will take Singular to compute the MFR of a graph.
}
\usage{
estimate.mfr.time(g)
}
\arguments{
  \item{g}{
a graph.
}
}
\details{
This is mostly only used for the \code{create.mfr.database} functions,
and so is generally not meant to be executed by the user. It provides
a very crude estimate of how long Singular might take to compute the MFR
of the graph, and should only be used as a guideline, and is pretty much
irrelevant if you don't have Singular installed.
}
\value{
seconds.
}
\author{
David J. Marchette, \email{dmarchette@gmail.com}.
}

\seealso{
\code{\link{create.mfr.database}}.
}
\examples{
g <- erdos.renyi.game(30,.1)
estimate.mfr.time(g)
}
\keyword{ graphs }
