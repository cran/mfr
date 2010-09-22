\name{neighborhood}
\alias{neighborhood}
\title{
Neighborhood of a vertex or vertices.
}
\description{
Computes the 1-neighborhood (open or closed) of a vertes. If given
a set of vertices, computes the vertices adjacent to these.
}
\usage{
neighborhood(g, v, open = FALSE)
}
\arguments{
  \item{g}{
a graph.
}
  \item{v}{
a vertex or set of vertices.
}
  \item{open}{
logical. If TRUE, the neighborhood does not contain v.
}
}
\details{
The neighborhood of a vertex is the set of vertices adjacent to it.
If the neighborhood is open, the vertex is not in its neighborhood.
If \code{v} is a set of vertices, the neighborhood is the set of
vertices adjacent to the vertices in \code{v}.
}
\value{
A vector of the vertices in the neighborhood.
}
\references{
Douglas B. West, Introduction to Graph Theory, Prentice-Hall, 2001.
}
\author{
David Marchette, dmarchette@gmail.com
}

\examples{
g <- cycleGraph(8)
neighborhood(g,1)
# 1 2 8
neighborhood(g,1,open=TRUE)
# 2 8
}
\keyword{ math }
