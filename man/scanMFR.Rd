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
scanMFR(g, method="size",...)
}
\arguments{
  \item{g}{
a graph.
}
  \item{method}{
a character string indicating the method of neighborhood selection.
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
David Marchette, david.marchette@navy.mil
}
\seealso{
\code{\link{mfr}}
}
\keyword{ graphs }
