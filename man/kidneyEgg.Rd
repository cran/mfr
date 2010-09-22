\name{kidneyEgg}
\alias{kidneyEgg}
\title{
Kidney-egg or kappa graph.
}
\description{
An Erdos-Renyi graph in which a small number of vertices have a different
(usually higher)
probability of connection. Also allows the "egg" vertices to have a 
different connection probability to the "kidney" vertices.
}
\usage{
kidneyEgg(n, p, m, q, r = p)
}
\arguments{
  \item{n}{
order of the graph.
}
  \item{p}{
edge probability within the kidney, or larger part of the graph.
}
  \item{m}{
order of the egg. Assumed to be less than n/2, but this is not checked.
}
  \item{q}{
edge probability within the egg.
}
  \item{r}{
edge probability between kidney and egg.
}
}
\details{
A kappa graph or kidney-egg graph is an 
Erdos-Renyi graph in which a small number of vertices have a different
(usually higher)
probability of connection. This also allows the "egg" vertices to have a 
different connection probability to the "kidney" vertices.
}
\value{
a graph.
}
\references{
Rukhin and Priebe, "A comparative power analysis of the maximum degree
and size invariants for random graph inference", to appear JSPI, 2010.
}
\author{
David Marchette, dmarchette@gmail.com
}

\seealso{
\code{\link{rg}}
}
\examples{
g <- kidneyEgg(100,.1,10,.6)
n <- 100
m <- 10
thetak <- seq(0,2*pi,length=n-m+1)[-1]
thetae <- seq(0,2*pi,length=m+1)[-1]
plotGraph(g,x=rbind(cbind(cos(thetak),sin(thetak)),
                    cbind(cos(thetae)+3,sin(thetae))))
}
\keyword{ math }
