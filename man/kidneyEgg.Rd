\name{kidney.egg.game}
\alias{kidney.egg.game}
\alias{kidneyEgg}
\title{
Kidney-egg or kappa graph.
}
\description{
An Erdos-Renyi graph in which a small number of vertices have a different
(usually higher)
probability of connection. Also allows the "egg" vertices to have a 
different connection probability to the "kidney" vertices. Alternatively,
the number of edges can be specified for any of the kidney, egg, or
kidney-to-egg sets.
}
\usage{
kidney.egg.game(n=100,p=.1,m=10,q=.4,r=p,g,h,ktype="gnp",etype="gnp",ektype="gnp",mode="out",directed=FALSE,loops=FALSE,add.to=FALSE)
}
\arguments{
  \item{n}{
order of the graph, if \code{g} is missing. This is ignored if \code{g}
is given.
}
  \item{g,h}{
\code{g} is the graph in which to add an egg. 
\code{h} is the egg graph. 
If either of these is missing, an Erdos-Renyi random graph is
generated. Note that you must have \code{vcount(h)<=vcount(g))}.
The only time it makes sense for \code{vcount(h)==vcount(g))} is
if \code{add.to} is TRUE.
}
  \item{p}{
edge probability within the kidney, or larger part of the graph.
If \code{ktype="gnp"} then this is an integer indicating the number of
edges in the kidney, which must be less than or equal to \code{choose(n-m,2)}.
This is ignored if \code{g} is given.
}
  \item{m}{
order of the egg.
}
  \item{q}{
edge probability within the egg.
If \code{etype="gnm"} then this is an integer indicating the number of
edges in the egg, which must be less than or equal to \code{choose(m,2)}.
This is ignored if \code{h} is given.
}
  \item{r}{
edge probability between kidney and egg. If this is missing then r=p.
If \code{ektype="gnm"} then this is an integer indicating the number of
edges, which must be less than or equal to \code{m*(n-m)}.
}
\item{ktype,etype,ektype}{
   either "gnp" or "gnm", see the manual page for \code{erdos.renyi.game}.
}
\item{directed,loops}{
as in \code{erdos.renyi.game}.
}
\item{mode}{
   passed to \code{graph.full.bipartite} to generate the edges between the
	kidney and the egg.
}
\item{add.to}{
   logical. If \code{g} is not given this is ignored. Otherwise, if it is
	TRUE, the resulting graph will have \code{m+vcount(g)} vertices and 
	the subgraph from vertex \code{m} on will be \code{g}. Otherwise the
	subgraph on the first \code{m} vertices will be replaced by the egg,
	with the kidney-egg edges addjusted according to \code{r} and 
	\code{ektype}.
}
}
\details{
A kappa graph or kidney-egg graph is an 
Erdos-Renyi graph in which a small number of vertices have a different
(usually higher)
probability of connection. This function 
also allows the "egg" vertices to have a 
different connection probability to the "kidney" vertices, if \code{r}
is given.
If \code{g} is given, an egg is added to the graph. It is important to
realize that the egg is always contained in the vertices 0,...,m-1,
so if \code{g} is given, you must organize it so that this is what
you expect: if \code{add.to} is true, the full graph \code{g} is
considered to be the kidney and the egg is added to it, otherwise
the first $m$ vertices are removed from \code{g} and replaced with the
egg. If either \code{g} or \code{h} are given, then the associated
parameters (\code{n,p,ktype} or \code{m,q,etype}) are ignored.
Note that if \code{directed} is TRUE, the graphs \code{g} and \code{h},
if given, should also be directed, otherwise you probably won't get
what you expect.

These graphs are given a layout that makes the structure fairly evident
when plotted.

\code{kidneyEgg} is an alias to this function.
}
\value{
a graph.
}
\references{
Rukhin and Priebe, "A comparative power analysis of the maximum degree
and size invariants for random graph inference", to appear JSPI, 2010.
}
\author{
David Marchette, david.marchette@navy.mil
}
\seealso{
   \code{\link{erdos.renyi.game}}, \code{\link{graph.full.bipartite}}.
}

\examples{
# a standard kappa graph
g <- kidneyEgg(n=100,p=.1,m=10,q=.6)

# a kidney-egg graph in which the egg has 10 edges.
h <- kidney.egg.game(n=100,p=.1,m=10,q=10,etype="gnm")
}
\keyword{ graphs }
