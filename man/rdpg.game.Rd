\name{rdpg.game}
\alias{rdpg.game}
\alias{rdpg.estimate}
\title{
Random Dot Product Graphs.
}
\description{
Generate a random dot product graph from vectors.
}
\usage{
rdpg.game(x, y = NULL, n = 100, d = 2, directed = FALSE, use.x = FALSE)
rdpg.estimate(g, d = 2)
}
\arguments{
  \item{x,y}{
matrices containing the vectors as rows.
}
  \item{n,d}{
  if \code{x} or the graph is directed and \code{y} is missing, 
  these are the number of vertices and dimension
  of the generated vectors. Otherwise these are ignored.
}
  \item{directed}{
  logical. Whether the graph is directed or not.
}
  \item{use.x}{
  logical. If the graph is directed, use the \code{x} 
  vectors in place of \code{y}. In this case \code{y} is ignored.
  Ignored if \code{directed} = FALSE.
}
\item{g}{
a graph.
}
}
\details{
A random dot product graph on the vectors \code{x} is an independent
edge graph with probabilities the dot products of the vectors. If 
\code{directed} is TRUE, the graph is directed, with probabilities
equal to the dot products of the \code{x} and \code{y} vectors.
Prior to generating the graph, any probabilities outside of [0,1]
are thresholded.

If \code{x} is missing, it is generated uniformly interior
to the \code{d+1}-dimensional simplex. Similarly if \code{directed}
is TRUE and \code{y} is missing, it is generated in the same manner.
In these cases the probabilities are guaranteed to be in [0,1].

If \code{directed} is FALSE, \code{y} is ignored, even if it is given.
}
\value{
\code{rdpg.game} returns
a graph, with graph attribute \code{x}, and, if directed, \code{y},
\code{y}. 

\code{rdpg.estimate} returns a list containing:
\item{x}{the estimated vectors.}
\item{values}{the singular- or eigen-values, depending on if the
graph is directed or not.}
}
\note{
The estimated vectors are only unique up to rotation, and so if you
want to compare them to the true values you must either compare the
two interpoint distance matrices (or other
rotation-invariant comparison), or Procrustes the vectors into the
same space.
}
\references{
D.J. Marchette and C.E. Priebe, "Predicting unobserved links in incompletely 
observed networks", Computational Statistics and Data Analysis, 52, 
1373--1386, 2008.
}
\author{
David J. Marchette, \email{dmarchette@gmail.com}.
}

\examples{
   g <- rdpg.game()
	est <- rdpg.estimate(g)
	sum(abs(dist(g$x)-dist(est$x)))/choose(vcount(g),2)
	\dontrun{
	   plot(g$x,pch=20)
		points(procrustes(est$x,g$x)$X.new,pch=20,col=2)
	}
}

\keyword{ graphs }
