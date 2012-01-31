\name{splittingEdge}
\alias{splittingEdge}
\alias{splittingEdges}
\title{
Splitting edge
}
\description{
Finds a splitting edge, or all the splitting edges, in a graph.
}
\usage{
splittingEdge(g)
splittingEdges(g)
}
\arguments{
  \item{g}{
a graph.
}
}
\details{
A splitting edge is an edge \code{uv} such that the closed neighborhood
of \code{u} contains the closed neighborhood of \code{v} (or vice versa).
}
\value{
an index into the edge list, or a vector of indices.
}
\references{
Huy Tai Ha and Adam Van Tuyl, "Splittable ideals and the resolutions of monomial
ideals", J. Algebra, 309, 405-425, 2007.
}
\author{
David Marchette, david.marchette@navy.mil
}

\keyword{ graphs }
