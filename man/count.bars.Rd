\name{count.bars}
\alias{count.bars}
\alias{count.triangles}
\alias{count.angles}
\title{
Count the number of induced subgraphs of particular types.
}
\description{
This counts the number of triangles, angles (incomplete triangles)
and bars (induced subgraphs on four vertices which have exactly two
edges and do not form an angle).
}
\usage{
count.bars(g)
count.triangles(g)
count.angles(g)
}
\arguments{
  \item{g}{
a graph.
}
}
\details{
this counts induced subgraphs on three vertices that are triangles or
angles, or on four vertices that are two separate edges.
}
\value{
the number of induced subgraphs of the particular type.
}
\author{
David J. Marchette, dmarchette@gmail.com
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
mfr(g)
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
