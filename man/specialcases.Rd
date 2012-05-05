\name{specialcases}
\alias{specialcases}
\alias{MFRComplete}
\alias{MFRCompleteBipartite}
\alias{MFRCycle}
\alias{MFRPath}
\alias{MFRbwg}
\alias{MFRStar}
\alias{MFRWheel}
\title{
Minimal free resolutions for special graphs.
}
\description{
Compute minimal free resolutions for special graphs for which the
resolutions are known and do not require complex calculations.
}
\usage{
MFRComplete(n)
MFRCompleteBipartite(n,m)
MFRCycle(n)
MFRbwg(spokes)
MFRStar(n,...)
MFRWheel(n)
}
\arguments{
  \item{n}{
order of the graph. In the case of a wheel graph, n must be odd.
In the case of a complete bipartite graph, this is the number of vertices
in the first group. In the case of \code{MFRStar}, this can be a graph,
in which case the code computes the MFR of the star of the graph.
}
  \item{m}{
number of vertices in the second group of the complete bipartite graph.
}
  \item{spokes}{
number of spokes in the broken wheel graph.
}
  \item{\dots}{
arguments passed to \code{mfr} if \code{n} is a graph.
}
}
\details{
For some graphs, the minimal free resolution is known.
\code{mfr} checks for some of these and uses the known formulas,
but we include them as directly callable here.
\code{MFRComplete} computes the MFR of a complete graph.
Similarly for \code{MFRCompleteBipartite},
\code{MFRCycle}, \code{MFRPath}, \code{MFRStar}
and \code{MFRWheel}. 
\code{MFRbwg} computes the MFR of a broken wheel graph.
Note that \code{MFRWheel} is a special case:
it is closed form only if \code{n} is odd. So this function should only
be called if \code{n} is odd.
}
\value{
a minimal free resolution, as returned by \code{mfr}.
}
\references{
Sean Jacques, "Betti numbers of graph ideals", PhD Thesis, University of
Sheffield, 2004.

Huy Tai Ha and Adam Van Tuyl, "Splittable ideals and the resolutions of monomial
ideals", J. Algebra, 309, 405-425, 2007.

David A. Johannsen and David J. Marchette, "Splitting Cycles in Graphs", 
preprint, 2010.

Marchette and Johannsen, "Betti numbers of graphs", submitted for publication
in Statistical Analysis and Data Mining.
}
\author{
David Marchette, \email{dmarchette@gmail.com}
}

\seealso{
\code{\link{mfr}}
}
\keyword{ graphs }
