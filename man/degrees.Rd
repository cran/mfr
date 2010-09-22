\name{degrees}
\alias{degrees}
\title{
The degree sequence of a graph.
}
\description{
Compute the degrees of the vertices of a graph.
}
\usage{
degrees(g)
}
\arguments{
  \item{g}{
a graph.
}
}
\details{
returns the degrees of the vertices of a graph.
}
\value{
a vector containing the degrees.
}
\references{
Douglas B. West, Introduction to Graph Theory, Prentice-Hall, 2001.
}
\author{
David Marchette, dmarchette@gmail.com
}
\examples{
g <- wheelGraph(20)
degrees(g)
# 3  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3 19
}
\keyword{ math }
