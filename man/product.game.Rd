\name{product.game}
\alias{product.game}
\title{
Randomly connect two graphs.
}
\description{
A graph which is the disjoint union of the two graphs, with random edges
added between them.
}
\usage{
product.game(g, h, p, n)
}
\arguments{
  \item{g,h}{
graphs. Both must be given.
}
  \item{p}{
  the edge probability. If \code{p} is given, \code{n} is ignored.
}
  \item{n}{
the number of edges to add.
}
}
\details{
Either \code{n} or \code{p} must be given. The resulting graph is
the disjoint union of the two graphs with either \code{n} edges
between them, or edges between them drawn with probability \code{p}.
\code{p} takes precedent, so if it is given, \code{n} is ignored.
}
\value{
a graph.
}
\author{
David J. Marchette, \email{dmarchette@gmail.com}
}
\seealso{
\code{\link{kidney.egg.game}}.
}
\examples{
g <- product.game(graph.ring(6),graph.ring(5),p=.4)
h <- product.game(graph.ring(6),graph.ring(5),n=7)
}
\keyword{ graphs }
