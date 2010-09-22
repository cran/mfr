\name{mfr-internal}
\alias{combineGraded}
\alias{mfr1}
\alias{has.cycle}
\title{
Internal functions for mfr package
}
\description{
Internal functions that should not be called by the user.
}
\usage{
combineGraded(gra,grb)
has.cycle(g,v)
mfr1(g,verbose=TRUE,nocode=FALSE,seed=33632)
}
\arguments{
    \item{gra,grb}{
	   graded betti numbers.
    }
    \item{g}{
	    a graph.
	 }
    \item{v}{
	    a vertex.
	 }
    \item{verbose}{
	    logical. Should stuff print as the function runs.
    }
	\item{nocode}{
	   logical. If TRUE Singular is not to run. The results will be
		approximate if the graph is not chordal.
	}
	\item{seed}{
		seed for the random number generator.
	}
}
\seealso{
\code{\link{mfr}}, \code{\link{rtree}}.
}
\author{
    David Marchette dmarchette@gmail.com
}
\keyword{math}
