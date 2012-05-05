\name{count.bars}
\alias{count.bars}
\alias{count.triangles}
\alias{count.angles}
\alias{count.squares}
\alias{count.bipartite}
\title{
Count the number of induced subgraphs of particular types.
}
\description{
This counts the number of triangles, angles (incomplete triangles),
bars (induced subgraphs on four vertices which have exactly two
edges and do not form an angle) and squares in a graph.
Also counts the number of induced Knm.
}
\usage{
count.bars(g)
count.triangles(g)
count.angles(g)
count.squares(g,induced=TRUE)
count.bipartite(g,m,n)
}
\arguments{
  \item{g}{
a graph.
}
  \item{m,n}{
the number of vertices of each type in a bipartite graph.
}
  \item{induced}{
logical. If TRUE, only induced squares (4-cycles with no chords) are
counted. If FALSE, any 4-cycle in the graph is counted (whether or not
the induced subgraph has one or more chords).
}
}
\details{
this counts induced subgraphs on three vertices that are triangles or
angles, or on four vertices that are two separate edges. The squares
are counted as either induced subgraphs, or as 4-cycles without consideration
of chords, depending on the value of \code{induced}.
}
\value{
the number of induced (or not) subgraphs of the particular type.
}
\author{
David J. Marchette, \email{dmarchette@gmail.com}
}

\examples{
set.seed(12345)
g <- rchordal(10,20)
count.triangles(g)
# [1] 14
count.angles(g)
# [1] 22
count.bars(g)
# [1] 49
mfr(g,suppress.warning=TRUE) 
#Minimal Free Resolution:
#Total betti numbers:
#	 1	 20	 99	 254	 381	 344	 185	 55	 7
#
#Graded:	 1	 0	 0	 0	 0	 0	 0	 0	 0
#	 0	 20	 50	 49	 22	 4	 0	 0	 0
#	 0	 0	 49	 204	 355	 334	 181	 54	 7
#	 0	 0	 0	 1	 4	 6	 4	 1	 0

# note that betti[2,3] = count.angles(g)+2*count.triangles(g)
# and betti[3,3] = count.bars(g)

}
\keyword{ graphs }
