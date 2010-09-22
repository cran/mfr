\name{remove.vertices}
\alias{remove.vertices}
\alias{isolates}
\alias{delete.edge}
\alias{removeIsolates}
\title{
Remove vertices from a graph.
}
\description{
Removes vertices from a graph, returning the induced subgraph containing
the remaining vertices.
}
\usage{
remove.vertices(g, v)
isolates(g, n)
delete.edge(g,edge)
removeIsolates(g)
}
\arguments{
  \item{g}{
a graph.
}
  \item{n}{
order of the graph.
}
  \item{edge}{
the edge (a vector of length 2) to be deleted.
}
  \item{v}{
a vector of vertices to be removed.
}
}
\value{
The induced subgraph on V-v. \code{removeIsolates} returns a graph
with the vertices renumbered so that there are no isolated vertices.
The \code{isolates} of a graph are the vertices with no edges. If
n is not given, the assumption is that the maximum vertex index is
the order of the graph.
}
\author{
David Marchette, dmarchette@gmail.com
}
\note{
   We assume that there are no isolated vertices with vertex number
	larger than any appearing in the graph, unless the order is given
	explicitly.
}

\examples{
g <- wheelGraph(10)
h <- remove.vertices(g,10)
is.cycle(h)
# TRUE
}
\keyword{ math }
