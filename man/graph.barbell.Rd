\name{graph.barbell}
\alias{graph.barbell}
\title{
Barbells.
}
\description{
A graph consisting of two complete graphs (or cycles) connected by a path.
}
\usage{
graph.barbell(n1 = 4, n2 = 4, p = 1, full = TRUE)
}
\arguments{
  \item{n1,n2}{
the number of vertices in the two "bells".
}
  \item{p}{
the length (number of edges) in the path.
}
  \item{full}{
     logical. If TRUE the bells are complete graphs, otherwise they are
	  cycles.
  }
}
\value{
a graph.
}
\author{
David J. Marchette, \email{dmarchette@gmail.com}.
}
\keyword{ graphs }
