\name{graph.truncated.tetrahedron}
\alias{graph.truncated.tetrahedron}
\alias{graph.martini.glasses}
\title{
Truncated Tetrahedron.
}
\description{
A 12 vertext graph that looks like a soccer ball.
}
\usage{
graph.truncated.tetrahedron(vertex.colors = c(2, 4), edge.colors = c(1, 3))
graph.martini.glasses(vertex.colors = c(2, 4), edge.colors = c(1, 3))
}
\arguments{
  \item{vertex.colors,edge.colors}{
vectors of two colors.
}
}
\details{
These are two graphs that are neighborhood colorable: every neighborhood
contains the same number of vertex/edge colors.
}
\value{
a graph.
}
\references{
David J. Marchette, Sul-Young Choi, Andrey Rukhin and Carey E. Priebe,
"Neighborhood Homogeneous Labelings of Graphs".
}
\author{
David J. Marchette, \email{dmarchette@gmail.com}.
}

\seealso{
\code{\link{clique.expand}}.
}
\keyword{ graphs }
