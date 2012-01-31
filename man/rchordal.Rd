\name{rchordal}
\alias{rchordal}
\alias{rchordal.add}
\alias{rchordal.subtract}
\title{Random Chordal Graphs
}
\description{
Generate a random chordal graph, either of a given order and size,
or from a graph by adding or removing edges.
}
\usage{
rchordal(n, size = n-1, g, from.tree=TRUE,from.ring=FALSE,add=TRUE, random.size=FALSE)
rchordal.add(g)
rchordal.subtract(g)
}
\arguments{
  \item{n}{
order of the graph.
}
  \item{size}{
size of the chordal graph.
}
  \item{g}{
initial graph.
}
  \item{from.tree,from.ring,add,random.size}{
logical. These control how the random graph is initialized and computed.
See the details below.
}
}
\details{
\code{rchordal} generates a random chordal graph
of order \code{n} and size \code{size}. If \code{random.size} is TRUE,
the size of the graph is chosen uniformly from \code{(n-1):choose(n,2)}.
If \code{g} is given, then it is used as an initial graph, and edges are
added or subtracted from \code{g} according to the value of \code{add}
until a chordal graph is obtained. In this case \code{size}, \code{random.size}
and the other logicals are ignored.
Otherwise, if \code{from.tree} is
TRUE, a random tree is used as the initial graph, and edges are added,
keeping the graph chordal, until the graph has size equal to \code{size}.
Otherwise, if \code{from.ring} is true, the graph starts with a cycle,
and edges are added at random until the graph is chordal.
}
\value{
a chordal graph.
}
\author{
David Marchette, david.marchette@navy.mil
}

\seealso{
\code{\link{rtree}}.
}
\examples{
   g <- rchordal(n=10,size=25)
	h <- graph.star(10)
	g <- rchordal.add(h)
	g <- rchordal.subtract(h)
}
\keyword{ graphs }
