\name{Order}
\alias{Order}
\alias{Size}
\alias{V}
\title{
Simple statistics of a graph.
}
\description{
Compute the order (number of vertices) and size (number of edges) of a graph.
Also returns the list of vertices in the graph.
}
\usage{
Order(g)
Size(g)
V(g)
}
\arguments{
  \item{g}{
a graph represented as an edge-list of pairs of vertices.
}
}
\details{
The order is the number of vertices, the size is the number of edges.
The empty graph has order and size both 0, and is represented as NULL.
\code{V} returns a vector of the unique vertices in \code{g} that are
not isolates.
}
\value{
an integer.
}
\author{
David J. Marchette, dmarchette@gmail.com
}

\examples{
g <- cbind(c(1,1,1,2,2,3,4,4,5),
           c(2,3,4,3,6,5,6,7,8))
Order(g)
# 8
Size(g)
# 9
V(g)
# 1 2 3 4 5 6 7 8
}
\keyword{ math }
