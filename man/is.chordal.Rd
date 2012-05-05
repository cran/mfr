\name{is.chordal}
\alias{peo}
\alias{is.chordal}
\alias{is.cycle}
\alias{is.complete}
\alias{simplicial.vertex}
\title{
Tests for certain types of graphs.
}
\description{
Tests that a graph is chordal or complete.
}
\usage{
is.chordal(g)
peo(g)
simplicial.vertex(g,n)
is.complete(g)
is.cycle(g)
}
\arguments{
  \item{g}{
a graph.
}
  \item{n}{
order of the graph.
}
}
\details{
A simplicial vertex is a vertex whose neighborhood is the complete graph.
A PEO is a partial elimination ordering, a list of vertices such that
each is simplicial in the remaining graph. A graph is chordal if and
only if it has a peo. If the graph is not chordal, \code{peo} will
return a partial list of simplicial vertices. 
}
\value{
\code{peo} returns a list of vertices. The others return a logical
indicating whether the graph is chordal/complete etc.
}
\references{
Douglas B. West, Introduction to Graph Theory, Prentice-Hall, 2001.
}
\author{
David Marchette, \email{dmarchette@gmail.com}
}
\note{
If the graph is not chordal, \code{peo} still returns a list of vertices.
This list will be less than the order of the graph, and is not unique
(an isomorphic graph may well return a different list). All that can
be assumed is that if the graph is not chordal, the list will not have
length the order of the graph.
}

\keyword{ graphs }
