
splittingEdge <- function(g)
{
   n <- vcount(g)
	s <- ecount(g)
	edges <- get.edgelist(g)+1

	retval <- .C("SplittingEdge",
					 edges1 = as.integer(edges[,1]),
					 edges2 = as.integer(edges[,2]),
					 N = as.integer(n),
					 S = as.integer(s),
					 edge = integer(1),
					 PACKAGE="mfr")
	 retval$edge+1
}

splittingEdges <- function(g)
{
   n <- vcount(g)
	s <- ecount(g)
	if(s==0) return(rep(0,n))
	edges <- get.edgelist(g)+1

	retval <- .C("SplittingEdges",
					 edges1 = as.integer(edges[,1]),
					 edges2 = as.integer(edges[,2]),
					 N = as.integer(n),
					 S = as.integer(s),
					 splitting = integer(s),
					 PACKAGE="mfr")
	 retval$splitting
}

