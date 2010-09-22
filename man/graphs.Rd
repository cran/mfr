\name{graphs}
\alias{graphs}
\alias{completeGraph}
\alias{completeBipartiteGraph}
\alias{cycleGraph}
\alias{ladderGraph}
\alias{pathGraph}
\alias{petersen}
\alias{petersenX}
\alias{starGraph}
\alias{wheelGraph}
\alias{brokenWheelGraph}
\alias{Star}
\title{
Well-known graphs.
}
\description{
Generate several well known graphs.
}
\usage{
completeGraph(n)
completeBipartiteGraph(n,m)
cycleGraph(n)
ladderGraph(n)
pathGraph(n)
petersen()
petersenX()
starGraph(n)
wheelGraph(n)
brokenWheelGraph(spokes)
Star(g)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n}{
order of the graph. In the case of the bipartite graph, order of one of
the two parts.
}
  \item{m}{
order of the second part of the bipartite graph.
}
  \item{spokes}{
number of spokes present in the wheel.
}
  \item{g}{
  a graph.
}
}
\details{
A cycle is a connected graph in which each vertex has degree 2.
a path is a cycle with one edge removed.
The complete graph is the graph with all possible edges, and the
complete bipartite graph on n and m vertices is has all possible edges
between the set of n vertices and the set of m vertices, with no edges
with each set.
A ladder is two paths connected by spokes (like a ladder).
\code{Star(g)} is a graph built from \code{g} and a vertex \code{v}
with edges from \code{v} to each vertex in \code{v}. \code{starGraph}
can be thought of as \code{Star} on the empty graph of n-1 vertices.
A wheel graph is \code{Star} applied to a cycle.
A broken wheel graph is a wheel graph with every other spoke missing.
Thus, it has 2*spokes+1 vertices.
The petersen graph is a well-known graph on 10 vertices. \code{petersenX}
returns the coordinates for the usual display of the petersen graph.
}
\value{
a graph.
}
\references{
Douglas B. West, Introduction to Graph Theory, Prentice-Hall, 2001.
}
\author{
David Marchette, dmarchette@gmail.com
}
\seealso{
\code{\link{rg}}
}
\keyword{ math }
