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
rchordal(n, size)
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
}
\details{
\code{rchordal} generates a random chordal graph
of order \code{n} and size \code{size}.
Given a graph \code{g} 
edges are added to or subtracted from \code{g} 
at random until the graph is chordal. 
}
\value{
a chordal graph.
}
\author{
David Marchette, dmarchette@gmail.com
}

\seealso{
\code{\link{rtree}}, \code{\link{rg}},\code{link{chordal.add}},
\code{\link{chordal.subtract}},\code{\link{chordal.sandwich}}.
}
\examples{
   g <- rchordal(n=10,size=25)
	h <- wheelGraph(10)
	g <- rchordal.add(h)
	g <- rchordal.subtract(h)
}
\keyword{ math }
