\name{mfr.undocumented}
\alias{checkDB1}
\alias{chordalComp1}
\title{Undocumented and unsupported mfr functions}
\description{These functions 
  are used internally by the C functions. They are not expected
  to be called by the user, but need to be exported so that the
  code can access them.
}
\usage{
checkDB1(z)
chordalComp1(z)
}
\arguments{
  \item{z}{a vector.}
}
\value{a minimal free resolution, if possible.}
\author{David J. Marchette, \email{dmarchette@gmail.com}}
\keyword{graphs}
