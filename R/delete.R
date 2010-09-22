
delete.edge <- function(g,edge)
{
	b <- intersect(which(g[,1] %in% edge),which(g[,2] %in% edge))
	if(length(b)==0) return(g)
   a <- g[-b,]
	if(length(a)==2)
	   a <- matrix(a,ncol=2)
	a
}

