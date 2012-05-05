\name{scanMFR}
\alias{scanMFR}
\alias{meanScanMFR}
\alias{uniqueScanMFR}
\title{
Minimal free resolutions of scan regions.
}
\description{
Computes the MFR of the neighborhood of each vertex.
}
\usage{
scanMFR(g, method="size",by.vertex=FALSE,
		  linear.strand=FALSE, maxBetti=vcount(g),
        ...)
}
\arguments{
  \item{g}{
a graph.
}
  \item{method}{
a character string indicating the method of neighborhood selection.
This is either one of "size","order","maximum","minimum", or the
name of a graph invariant. If the latter, \code{method} must take a
single parameter called "graph".
}
  \item{by.vertex}{
logical. If TRUE, \code{method} is applied to the overall graph,
returning a vector containing a single value for each vertex. Otherwise,
\code{method} is applied to each neighborhood.
}
  \item{linear.strand,maxBetti}{
if \code{linear.strand} is true, only \code{maxBetti} numbers of the linear strand are computed.
}
  \item{\dots}{
arguments passed to \code{mfr}
}
}
\details{
Computes the MFR of the scan region (neighborhood) of each vertex.
If \code{method="size"} the number of edges in the neighborhood
is used to determine the subgraph for which the mfr is computed.
If \code{method="order"} the largest neighborhood is used. 
If \code{method="maximum"} or \code{"minimum"}
then every neighborhood is computed, and the result
is the maximum/minimum 
value for each betti number (total or graded) over all the
regions. In this case, the total betti numbers will NOT be the sum of the
columns of the graded betti numbers.

If method is not one of the above, it is assumed to be a graph invariant
that takes a single parameter, "graph", and returns a number or vector. If 
\code{by.vertex} is true, then \code{method} is applied to the full graph
and is assumed to return a vector with a single value for each vertex.
If \code{by.vertex} is FALSE, \code{method} is applied to each neighborhood,
and is assumed to return a single value. In either case, the local
region is defined to be the neighborhood of the vertex
with the largest value.
}
\value{
\code{scanMFR} returns
the minimal free resolution of the neighborhood determined by
the locality test according to the values of \code{by.size} and \code{by.order},
or the maximum bettis over all neighborhoods.
}
\references{
Huy Tai Ha and Adam Van Tuyl, "Splittable ideals and the resolutions of monomial
ideals", J. Algebra, 309, 405-425, 2007.

Marchette and Johannsen, "Betti numbers of graphs".
}
\author{
David Marchette, \email{dmarchette@gmail.com}
}
\seealso{
\code{\link{mfr}},
\code{\link{linearStrand}},
\code{\link{neighborhood}}.
}
\keyword{ graphs }
