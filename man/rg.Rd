\name{rg}
\alias{rg}
\title{
Erdos-Renyi random graph
}
\description{
Generate an Erdos-Renyi (Bernouli) random graph.
}
\usage{
rg(n, p = 0.5)
}
\arguments{
  \item{n}{
order of the graph.
}
  \item{p}{
edge probability. May be a matrix.
}
}
\details{
This generates a random graph where edges are independently present
with probability \code{p}. If \code{p} is an nxn symmetric matrix, the
probability of an edge from \code{i} to \code{j} is \code{p[i,j]}.
The matrix is not checked for symmetry, and only the upper triangle is used.
}
\value{
a graph.
}
\references{
Bela Bollabas, Random Graphs, Cambridge, 2001.
}
\author{
David Marchette, dmarchette@gmail.com
}

\seealso{
\code{\link{graphs}}
}
\keyword{ math }
