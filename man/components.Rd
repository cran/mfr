\name{components}
\alias{components}
\title{
Connected components of a graph.
}
\description{
Compute the connected components of a graph.
}
\usage{
components(g)
}
\arguments{
  \item{g}{
a graph.
}
}
\value{
a vector of integers of length equal to the order of the graph. Each vertex
in a connected component has the same integer in the vector.
}
\author{
David Marchette, dmarchette@gmail.com
}

\examples{
g <- rbind(c(1,2),
           c(2,3),
			  c(4,5),
			  c(5,6))
components(g)
# 1 1 1 2 2 2
}
\keyword{ math }
