
splittingEdge <- function(g)
{
   n <- Order(g)
	s <- Size(g)

	retval <- .C("SplittingEdge",
					 edges1 = as.integer(g[,1]),
					 edges2 = as.integer(g[,2]),
					 N = as.integer(n),
					 S = as.integer(s),
					 edge = integer(1),
					 PACKAGE="mfr")
	 retval$edge+1
}

splittingEdges <- function(g)
{
   n <- Order(g)
	s <- Size(g)
	if(s==0) return(rep(0,n))

	retval <- .C("SplittingEdges",
					 edges1 = as.integer(g[,1]),
					 edges2 = as.integer(g[,2]),
					 N = as.integer(n),
					 S = as.integer(s),
					 splitting = integer(s),
					 PACKAGE="mfr")
	 retval$splitting
}

