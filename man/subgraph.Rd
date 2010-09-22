\name{subgraph}
\alias{subgraph}
\title{
Subgraph
}
\description{
Computes the induced subgraph of a graph.
}
\usage{
subgraph(g, U,remove=TRUE)
}
\arguments{
  \item{g}{
a graph.
}
  \item{U}{
a vector of vertices.
}
  \item{remove}{
  logical. If true, isolates are removed before returning the subgraph.
  This means that the vertex labels may no longer be the same.
}
}
\value{
a graph. The vertices may be renumbered, since isolates are removed.
}
\references{
Douglas B. West, Introduction to Graph Theory, Prentice-Hall, 2001.
}
\author{
David Marchette, dmarchette@gmail.com
}

\examples{
g <- completeGraph(10)
h <- subgraph(g,c(1,3,5))
is.complete(h)
# TRUE
is.cycle(h)
# TRUE
}
\keyword{ math }
