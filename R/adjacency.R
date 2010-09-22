adjacencyMatrix <- function(g)
{
   n <- Order(g)
	A <- matrix(0,nrow=n,ncol=n)
	A[g] <- 1
	A[cbind(g[,2],g[,1])] <- 1
	A
}
