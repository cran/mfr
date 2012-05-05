\name{mfrDB}
\alias{mfrDB}
\docType{data}
\title{
Database of small graphs and their minimal free resolutions.
}
\description{
A database containing some small graphs (n<=20) with their
minimal free resolutions. This can be checked to speed up the
computations. This contains the minimal free resolution for all
the graphs in \code{graph.atlas}, which are all graphs up to order
7. 
}
\usage{data(mfrDB)}
\format{
  A list containing the following attributes:
 \describe{
	 \item{\code{graphs}}{a list of igraph graphs.}
	 \item{\code{mfrs}}{a list of minimal free resolutions.}
	 \item{\code{orders}}{a vector containing the orders of the graphs.}
	 \item{\code{sizes}}{a vector containing the sizes of the graphs.}
	 \item{\code{timings}}{a list containing the timings of the MFR calculations.}
	 \item{\code{n}}{the number of graphs/mfrs in the database.}
  }

}
\details{
For each graph in \code{mfrDB$graphs} the minimal free resolution has been 
pre-computed and is contained in the corresponding entry of \code{mfrDB$mfrs}.
These have been calculated on a machine with Singular installed, so these
are the true minimal free resolutions. The timings are a guide to how long the
MFR took for each graph, but of course this is dependent on the machine on which
they were computed. These do provide a guideline for how long it takes to compute
MFRs, at least for small graphs.

The database shipped with the package by default is small. 
Other versions of the database are available at the author's web page. Go to
http://dmarchette.com/MFR/index.html to download other databases. These can
be loaded into an R session, replacing the existing \code{mfrDB}, or you
can replace the file mfr/data/mfrDB.RData with one of these and re-install the
package. Note that the bigger the database the longer the look-ups, so
there's a trade-off, as always.

For the most part, these graphs required Singular at the time of computation.

}
\seealso{
\code{\link{checkDB}}, \code{\link{mfr}}, \code{\link{scanMFR}}.
}
\keyword{datasets}
\keyword{graphs}
