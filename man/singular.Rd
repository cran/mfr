\name{singular}
\alias{singular}
\alias{has.singular}
\title{
Call Singular.
}
\description{
Calls Singular to return the minimal free resolution of the edge ideal.
}
\usage{
singular(g, verbose = FALSE)
has.singular()
}
\arguments{
  \item{g}{
a graph.
}
  \item{verbose}{
logical. Output stuff while running.
}
}
\details{
Singular is a computer algebra package that computes MFRs (among other things).
This code will call Singular. It is strongly advised that this never be used
on graphs of more than about 15 vertices. \code{has.singular} checks for
the availability of Singular on the system. This is called by \code{mfr}
if necessary.
}
\value{
a minimal free resolution.
}
\references{
Huy Tai Ha and Adam Van Tuyl, "Splittable ideals and the resolutions of monomial
ideals", J. Algebra, 309, 405-425, 2007.
}
\author{
David Marchette, dmarchette@gmail.com
}

\seealso{
\code{\link{mfr}}
}
\keyword{ math }
