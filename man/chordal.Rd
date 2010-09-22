\name{chordal}
\alias{chordal.sandwich}
\alias{chordal.add}
\alias{chordal.subtract}
\title{Chordal Graphs
}
\description{
Generate a chordal graph from a given graph.
}
\usage{
chordal.add(g)
chordal.subtract(g)
chordal.sandwich(g)
}
\arguments{
  \item{g}{
initial graph.
}
}
\details{
Given a graph \code{g} 
edges are added to or subtracted from \code{g} 
at random until the graph is chordal. 
\code{chordal.sandwich} returns
two graphs, one which is a chordal subgraph of \code{g} and one
which is a chordal supergraph of \code{g}. The difference between
these and their \code{rchordal} equivalents is that at each stage
these check all possible single edge additions or subtractions for
chordality before resorting to a random edge.

}
\value{
a chordal graph. In the case of \code{chordal.sandwich}, a list of two graphs:
\item{lower}{a chordal induced subgraph of \code{g}.}
\item{upper}{a chordal graph of which \code{g} is an induced subgraph.}
}
\author{
David Marchette, dmarchette@gmail.com
}

\seealso{
\code{link{rchordal.add}},
\code{\link{rchordal.subtract}}.
}
\examples{
	h <- wheelGraph(10)
	g1 <- chordal.add(h)
	g2 <- chordal.subtract(h)
	g3 <- chordal.sandwich(h)
}
\keyword{ math }
