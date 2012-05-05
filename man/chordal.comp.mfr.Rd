\name{chordal.comp.mfr}
\alias{chordal.comp.mfr}
\alias{is.chordal.comp}
\title{
Minimal Free Resolution of a graph whose complement is chordal.
}
\description{
If the complement of the graph g is chordal, then the resolution
is linear and there is a simple formula for it. This implements this
formula.
}
\usage{
chordal.comp.mfr(g,maxBetti=vcount(g))
is.chordal.comp(g)
}
\arguments{
  \item{g}{
a graph.
}
  \item{maxBetti}{
the maximum number of Betti numbers to compute.
}
}
\details{
Implements the algorithm described in the reference. If \code{maxBetti}
is given, it will compute at most this many columns of the graded Betti
numbers.
}
\value{
A minimal free resolution as returned by \code{mfr}.
}
\references{
Anton Dochtermann and Alexander Engstrom,
"Algebraic properties of edge ideals
via combinatorial topology", Electronic Journal of Combinatorics, 16 (2),
www.combinatorics.org/Volume_16/PDF/v16i2r2.pdf.
}
\author{
David J. Marchette, \email{dmarchette@gmail.com}
}

\seealso{
\code{\link{mfr}}, \code{\link{singular}}, \code{\link{linearStrand}}.
}
\examples{
set.seed(65354)
h <- rchordal(10,20)
g <- graph.complementer(h)
chordal.comp.mfr(g)
#Minimal Free Resolution:
#Total betti numbers:
#	 1	 25	 95	 171	 179	 115	 45	 10	 1
#
#Graded:	 1	 0	 0	 0	 0	 0	 0	 0	 0
#	 0	 25	 95	 171	 179	 115	 45	 10	 1

}
\keyword{ graphs }
