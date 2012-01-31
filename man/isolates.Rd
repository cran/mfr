\name{isolates}
\alias{isolates}
\alias{removeIsolates}
\title{
Isolated vertices.
}
\description{
Find or remove the isolated vertices in a graph.
}
\usage{
isolates(g)
removeIsolates(g)
}
\arguments{
  \item{g}{
a graph.
}
}
\details{
return a vector of the isolated vertices of a graph, or remove the isolated
vertices of a graph. This uses delete.vertices, and so the vertex ids are
not retained.
}
\value{
a graph or a vector of vertices.
}
\author{
David J. Marchette, dmarchette@gmail.com
}

\keyword{ graphs }
