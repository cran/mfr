\name{adjacencyMatrix}
\alias{adjacencyMatrix}
\title{
Adjacency matrix.
}
\description{
Converts a graph to an adjacency matrix.
}
\usage{
adjacencyMatrix(g)
}
\arguments{
  \item{g}{
a graph.
}
}
\details{
Returns a symmetric nxn matrix (where n is the order of the graph)
with a 1 in position i,j if and only if there is an edge between i and j.
}
\value{
a matrix.
}
\references{
Douglas B. West, Introduction to Graph Theory, Prentice-Hall, 2001.
}
\author{
David Marchette, dmarchette@gmail.com
}

\seealso{
\code{\link{plotAdjacency}}
}
\keyword{ math }
