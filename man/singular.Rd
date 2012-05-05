\name{singular}
\alias{singular}
\alias{singular.version}
\alias{has.singular}
\alias{graph2EdgeIdeal}
\title{
Call Singular.
}
\description{
Routines to interface with  Singular to return the minimal free resolution of 
the edge ideal, or to execute a given set of commands in Singular.
}
\usage{
singular(g, verbose = FALSE, command, quiet = FALSE)
graph2EdgeIdeal(g,resString)
singular.version(verbose = FALSE)
has.singular()
}
\arguments{
  \item{g}{
a graph.
}
  \item{verbose}{
logical. Output stuff while running, or provide a lot of version information.
}
  \item{quiet}{
logical. If TRUE no warnings printed when calling Singular for a 
"large" graph.
}
  \item{command}{
a command to pass to Singular for execution.
}
  \item{resString}{
the command Singular will use to compute the mfr.
}
}
\details{
Singular is a computer algebra package that computes MFRs (among other things).
This code will call Singular. It is strongly advised that this never be used
on graphs of more than about 20 vertices. \code{has.singular} checks for
the availability of Singular on the system. This is called by \code{mfr}
if necessary. If \code{g} is not given, then \code{command} can be set
to a character string to be executed by Singular. \code{singular.version}
returns a string containing version information, lots of it if \code{verbose}
is TRUE. \code{graph2EdgeIdeal} produces a string containing the Singular
code to compute the mimimal free resolution of the edge ideal of \code{g}.
\code{resString} must be one of "res","lres","mres","sres","nres", or
"minres".

I have not tested this on a Windows platform. Calling Singular
on a Windows box is quite different than Unix. At the very least,
the macro SINGULAR in singular.c probably needs to be changed.
If you figure out a way to get this to work without doing this, drop
me a line so I can fix the code. 
}
\value{
\code{singular} returns
a minimal free resolution. If \code{g} is not given and \code{command}
is a character string, this is executed by Singular, returning the 
output from Singular as a character vector. \code{singular.version}
returns a character vector containing version information.
}
\references{
Huy Tai Ha and Adam Van Tuyl, "Splittable ideals and the resolutions of monomial
ideals", J. Algebra, 309, 405-425, 2007.

Decker, W.; Greuel, G.-M.; Pfister, G.; Schonemann, H.: 
"Singular {3-1-3} --- A computer algebra system for polynomial computations."
http://www.singular.uni-kl.de, 2011.
}
\author{
David Marchette, \email{dmarchette@gmail.com}
}
\note{
  It is hoped that in a future release I will have C code to directly compute
  the MFR in the event that Singular is unavailable. Instead, if Singular
  is unavailable (and currently this is always the case on Windows, at least,
  even if Singular is installed, due to the vagueries of Windows) the results
  from the code are not correct, and one shouldn't rely on them (although they
  may be in the ballpark, if you are lucky). If the graph is chordal, this is
  never a problem, and even if it isn't there will be cases where the code
  can figure out the MFR without resorting to Singular. Bottom line: if you
  don't have Singular working with this code you should probably restrict
  to chordal graphs.

	The way the code works now, 
	\code{has.singular}
	will basically return FALSE on a Windows platform. Singular uses cygwin,
	and as near as I can figure, you need to be in the cygwin/bin directory
	and call "bash.exe Singular" in order to execute Singular. This still
	has some problems that I was unable to address, and so for now Singular
	is not callable on a Windows platform.

	On a Unix platform, as long as Singular is in your path, this should work.
}

\seealso{
\code{\link{mfr}}
}
\keyword{ graphs }
