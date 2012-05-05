\name{partial}
\alias{linearStrand}
\alias{linearStrandLower}
\alias{betti2}
\alias{betti3}
\title{
Functions to Compute a Few Betti Numbers.
}
\description{
Compute a few betti numbers such as the linear strand and other subsets.
}
\usage{
linearStrand(g,maxBetti=vcount(g),exact=FALSE)
linearStrandLower(g,maxBetti=vcount(g))
betti2(g)
betti3(g)
}
\arguments{
  \item{g}{
a graph.
}
  \item{maxBetti}{
the maximum number of Betti numbers to compute in the linear strand.
}
  \item{exact}{
logical. See details.
}
}
\details{
The linear strand of a graph is the first non-trivial row of the graded
resolution. \code{betti3} is the third column of the graded resolution.
\code{betti2} is an alias to \code{ecount}. The function \code{linearStrand}
is exact if the graph is free of induced chord-free $4$-cycles. 
If it contains a $4$-cycle, and \code{exact} is FALSE,
the Betti numbers returned are a lower bound on
the true betti numbers. If \code{exact} is TRUE, the calculation is
exact, but may take a very long time if \code{maxBetti} and/or the
graph is very large. 

The function \code{linearStrandLower} should only be called on a graph
containing an induced $4$-cycle. It does not check for this. Instead,
you should use \code{linearStrand}, which does check, and callse the
appropriate code.
}
\value{
a list with single element \code{graded} which is a 2 row matrix whose
second row is the linear strand. This is so that the linear strand 
"looks like" a minimal free resolution.
Its class tells whether the result is exact or a lower bound.
}
\note{
If \code{exact} is TRUE, then the returned linear strand is correct. If
it is false, it may or may not be correct, depending on whether it contains
induced
$4$-cycles. The code to compute the linear strand of a $4$-cycle-free graph
is MUCH faster than the code to compute the linear strand of a graph containing
a $4$-cycle exactly. If you do not call \code{linearStrand} with 
\code{exact=TRUE}, you will need to check to see if the result inherits
"exact" to determine if you have an exact result or a lower bound.
}
\references{
Ha and Van Tuyl, "Resolutions of square-free monomial ideals via facet
ideals: a survey", Contemporary Mathematics, 448, 2007, 
http://129.81.170.14/~tai/papers/facet_resolution_survey.pdf.
}
\author{
David J. Marchette, \email{dmarchette@gmail.com}.
}

\seealso{
\code{\link{mfr}}.
}
\examples{
   linearStrand(graph.famous("Coxeter"))
	# 1  0  0  0
	# 0 42 84 28
}
\keyword{ graphs }
