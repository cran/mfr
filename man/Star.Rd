\name{Star}
\alias{Star}
\alias{graph.wheel}
\alias{graph.bwg}
\title{
Star graphs.
}
\description{
This generalizes the idea of a star graph. 
}
\usage{
Star(x)
graph.wheel(n)
graph.bwg(n)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
Either a number indicating the number of vertices of degree 1 in the
resultant star graph, or a graph.
}
  \item{n}{the order of a "broken wheel" graph (must be odd).
}
}
\details{
If \code{x} is a number, then 
\code{Star(x) = graph.star(x+1,mode="undirected")}.
Otherwise, if \code{x} is a graph, a new node is added to the graph,
and edges are added from this node to every other node in the graph.
\code{Star(x)} is always an undirected simple graph. A broken wheel graph
is a wheel graph (\code{graph.wheel})
with an odd number of vertices in which every other
spoke in the wheel is missing.
}
\value{
a graph.
}
\author{
David J. Marchette, \email{dmarchette@gmail.com}
}

\seealso{
\code{\link{graph.star}}.
}
\examples{
g <- graph.star(4,mode="undirected")
h <- Star(3)
graph.isomorphic(g,h)

g <- graph.wheel(5)
h <- Star(graph.ring(4))
graph.isomorphic(g,h)

}
\keyword{ graphs }
