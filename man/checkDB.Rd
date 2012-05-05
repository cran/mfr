\name{checkDB}
\alias{checkDB}
\alias{which.DB}
\alias{combineDBs}
\title{
Check the database of minimal free resolutions.
}
\description{
For small graphs (n<=20) check the database of pre-computed minimal
free resolutions.
}
\usage{
checkDB(g)
which.DB(g,db)
combineDBs(db1,db2,verbose=FALSE)
}
\arguments{
  \item{g}{
a graph.
}
  \item{db,db1,db2}{
a list defaulting to mfrDB. This should generally not be used by the user.
}
  \item{verbose}{
  a logical.
}
}
\details{
This checks the database \code{mfrDB$graphs} for a graph 
isomorphic to \code{g}.
If found, the associated minimal free resolution (in \code{mfrDB$mfrs}) 
is returned.
The database is designed to reduce the number of times Singular needs to be
called. If checkDB is used prior to calling \code{mfr} (of if the
\code{check.database} flag is set to TRUE in the call to \code{mfr}),
then Singular will not ever be called on graphs of order less than 8.
In addition, there are graphs in the database of order up to 20, although
these larger graphs do not correspond to a complete list.

\code{combineDBs} merges two MFR databases, and returns the merged database.
The smaller database should be second. If it isn't, they are swapped, but
the swapping uses memory, so you should always call this function with the
larger database (more entries) first.
}
\value{
either a minimal free resolution, or NULL if the graph is not in the database.
\code{which.DB} returns an index into the database if the graph is in the
database, otherwhise it returns NULL.
}
\author{
David J. Marchette, \email{dmarchette@gmail.com}
}
\note{
the value \code{punted} is always set to zero on the returned mfr, even
if Singular was called to create the original database. This is so that
the user can see that her call to mfr did not require an execution of 
Singular. In the database \code{mfrDB$mfrs}, the attribute \code{punted}
does contain the number of calls to Singular, so one can look to see
whether the graph is one that could not be resolved without a call to
the external program.
}
\seealso{
\code{\link{mfr}}, \code{\link{mfrDB}}.
}
\examples{
   g <- graph.famous("Chvatal")
	checkDB(g)
   mfrDB$mfrs[[which.DB(g)]]

}
\keyword{ graphs }
