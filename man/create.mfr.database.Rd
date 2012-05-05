\name{create.mfr.database}
\alias{create.mfr.database}
\title{
Create the mfrDB database.
}
\description{
Runs mfr on a large number of graphs to create the database mfrDB.
}
\usage{
create.mfr.database(N = 1000, M = 20, 
						  seed=72368,
						  maxTimeR=1200,
						  maxTimeE=600,
						  maxTimeD=60,
						  reset=FALSE,
						  verbose=TRUE)
}
\arguments{
  \item{N}{
number of random graphs to create for each order.
}
  \item{M}{
maximum order of the random graphs.
}
  \item{seed}{
seed to the random number generator.
}
  \item{maxTimeR,maxTimeE,maxTimeD}{
numbers that control the maximum amount of time that the algorithms will spend on a given MFR. These
do not apply to the famous graphs, and are only a guideline, not a firm condition.
}
  \item{reset}{
logical. If TRUE, only the atlas and famous graphs are run.
Once this has completed, you should save the mfrDB.RData file
in mfr/data and re-install the package. This resets the database
to the basics.
}
  \item{verbose}{
logical. If TRUE, spit out information as it runs.
}
}
\details{
This first runs through all the \code{graph.atlas} graphs, and then
the \code{graph.famous} graphs (except the Coxeter graph and other large graphs) that aren't
in the atlas, as well as a few other standard types of graphs. It only
processes connected graphs, and graphs that require Singular to compute,
and so this part of the database contains
many fewer than the 1252 graphs in the atlas.
It then creates \code{N} \code{kidney.egg.game} graphs
with parameters n=15:20, p=0.1, m=5, for various values of q,
and random graphs with various values of p.
This is pretty arbitrary.
It then creates \code{N} graphs for each order from 
8 to \code{M} that start with a star graph centered at
vertex v=0, then adds one edge from
each neighbor of v to another neighbor (so that there are no pendants)
then add 0 or more edges
between the neighbors of v. This does not exhaust all such
graphs. It then generates a bunch of graphs from various random
graph classes, and does some other things to try to fill out the
database. There is always the trade-off between filling the database
and actually being able to compute the MFRs in our lifetime, so the
database will never be complete. Also, it refuses to compute the MFRs
for the graphs that will "take too long" (as defined by the various
\code{maxTime} parameters).
The downside is that it will tend not
to contain the very graphs that take a long time to compute, but 
the database still extends the number of non-chordal graphs that can
be processed without the need for Singular.

The graphs in the database (with a very small number of exceptions)
are ones which do require Singular to compute (at least, they did
at the time the database was created). Thus, while it is far from
complete, at least the graphs in the database are for the most part
ones which would have to be sent to Singular.
}
\value{
The result is stored in the file mfrDB.RData in the current directory.
This contains the variable \code{mfrDB}, which is a list containing
\item{graphs}{the graphs in the database (a list).}
\item{mfrs}{the associated minimal free resolutions (a list).}
\item{orders}{the number of vertices in each graph (a vector).}
\item{n}{the size of the database.}
}
\author{
David J. Marchette, \email{dmarchette@gmail.com}
}
\note{
The typical user is not expected to use this function. The main reason
for this database is to reduce the number of graphs that require calls
to Singular, which is an efficiency gain, but is also important for
users that do not have the Singular package installed. IMPORTANT: if
you do not have Singular installed and working with the package, you
should not call \code{create.mfr.database}. It will result in a corrupted
database, and would be pointless in any case.

Also, the changes will not take effect unless you save the mfrDB.RData file
in mfr/data and re-install the package (or load the mfrDB.RData directly
into your R session after loading mfr each time you use the package).
}

\seealso{
\code{\link{checkDB}}, and \code{\link{graph.star}},
\code{\link{erdos.renyi.game}}, \code{\link{kidney.egg.game}},
\code{\link{grg.game}}, \code{\link{rdpg.game}},
for some of the graph classes that are used to generate entries in the
database.
}
\keyword{ graphs }
