\name{directed2bipartite}
\alias{directed2bipartite}
\alias{layoutD2B}
\title{
Convert a Directed Graph
}
\description{
Converts a directed graph into a bipartite graph in which each vertex is
replace by two: an in-vertex and an out-vertex in order to encode the direction
of the edges.
}
\usage{
directed2bipartite(g, add.internal = TRUE, remove.isolates = TRUE)
layoutD2B(g,layout=get.graph.attribute(g,name="layout"),prop=1.1,
          twiddle=FALSE,as.bipartite=FALSE)
}
\arguments{
  \item{g}{
a directed graph.
}
  \item{add.internal}{
logical. If TRUE an edge is added between each in-vector and the associated
out-vector.
}
  \item{remove.isolates}{
logical. If TRUE isolated vertices are removed prior to processing. This
is important if \code{add.internal} is TRUE in order to avoid adding
spurious edges.
}
  \item{layout}{
a matrix of points associated with the graph \code{g}.
If \code{layout} is NULL, a circular layout is used.
}
  \item{prop,twiddle}{
\code{prop} is a proportion to move the out-vertices away from the in-vertices.
If 
\code{twiddle} is TRUE or a number, it adds a random twiddle 
to the out-vertices'
coordinates.
}
  \item{as.bipartite}{
logical. If TRUE, the layout is as a bipartite graph, with the in-vertices
plotted on the left and the out-vertices on the right.
}
}
\details{
\code{directed2bipartite} returns an undirected graph, in which each vertex
has been replicated so that there are now in-vertices and out-vertices 
associated with each original vertex. Each directed edge is then replace
with an undirected edge between the associated out-vertex and in-vertex.
In addition, if \code{add.internal} is TRUE, an additional edge is added
for each original vertex, connecting the associated in- and out-vertices.
Isolated vertices are removed unless otherwise directed.
}
\value{
a graph, or, in the case of \code{layoutD2B} a matrix of 2-dimensional points,
one for each in- and out-vertex.
}
\author{
David Marchette, \email{dmarchette@gmail.com}
}

\examples{
g <- graph.ring(10,directed=TRUE)
h <- directed2bipartite(g)
\dontrun{
plot(g)
plot(h,layout=layoutD2B(g),vertex.size=1)
plot(h,layout=layoutD2B(g,as.bipartite=TRUE),vertex.size=1)
}
}
\keyword{ graphs }
