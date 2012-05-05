\name{layout.concentric.circles}
\alias{layout.concentric.circles}
\title{
Concentric Circles Layout
}
\description{
Lay the graph out in concentric circles, which each circle containing
the neighbors of the interior circle.
}
\usage{
layout.concentric.circles(graph, params)
}
\arguments{
  \item{graph}{
a graph.
}
  \item{params}{
the only parameter for this function is \code{nodes} which defaults to 0.
}
}
\details{
If \code{nodes} contains a single vertex, this is centered at (0,0),
otherwise these vertices are layed out on a circle using \code{layout.circle}.
The neighborhood of the vertices which have been assigned coordinates
is then computed, and any new vertices are assigned coordinates on a new
circle. This continues until all the vertices have been assigned a coordinate
(if the graph is not connected, all vertices that have not yet been
assigned coordinates are put on the largest circle -- this means that
as a rule this function should not be used on graphs that are not connected).
}
\value{
a matrix of coordinates for the vertices.
}
\author{
David J. Marchette, \email{dmarchette@gmail.com}.
}

\seealso{
\code{\link{layout.circle}}.
}
\keyword{ graphs }
