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
scanMFR(g, open = FALSE, ...)
meanScanMFR(m)
uniqueScanMFR(m)
}
\arguments{
  \item{g}{
a graph.
}
  \item{m}{
a list of minimal free resolutions (the output of \code{scanMFR}).
}
  \item{open}{
logical. Is the neighborhood open?
}
  \item{\dots}{
arguments passed to \code{mfr}
}
}
\details{
Computes the MFR of the scan region (neighborhood) of each vertex.
\code{meanScanMFR} computes the average of the resolutions. 
\code{uniqueScanMFR} returns the number of unique MFR's in the list,
with a count indicating how many times each distinct MFR occurs.
}
\value{
\code{scanMFR} returns
a list of minimal free resolutions for each vertex. 
\code{meanScanMFR} returns a free resolution.
\code{uniqueScanMFR}
returns with entries
\item{graded}{a list of unique graded resolutions}
\item{count}{a vector of counts indicating the number of times the resolution
appears in the list \code{m}}
}
\references{
Huy Tai Ha and Adam Van Tuyl, "Splittable ideals and the resolutions of monomial
ideals", J. Algebra, 309, 405-425, 2007.

Marchette and Johannsen, "Betti numbers of graphs", submitted for publication
in Statistical Analysis and Data Mining.
}
\author{
David Marchette, dmarchette@gmail.com
}
\seealso{
\code{\link{mfr}},
\code{\link{plotScanMFR}}
}
\keyword{ math }
