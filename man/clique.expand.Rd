\name{clique.expand}
\alias{clique.expand}
\title{
Clique expand a graph.
}
\description{
Expands a d-regular graph by replacing each vertex with a d-clique.
}
\usage{
clique.expand(g = graph.full(4, directed = FALSE))
}
\arguments{
  \item{g}{
a graph.
}
}
\details{
Each vertex is replaced with a clique, and the edges are rewired
appropriately.
}
\value{
a graph.
}
\author{
David J. Marchette, \email{dmarchette@gmail.com}.
}
\seealso{
\code{\link{graph.truncated.tetrahedron}}.
}
\examples{
g <- graph.truncated.tetrahedron()
h <- clique.expand()
graph.isomorphic(g,h)
# TRUE
}
\keyword{ graphs }
