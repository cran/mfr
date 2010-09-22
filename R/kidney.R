kidneyEgg <- function(n,p,m,q,r=p)
{
   P <- matrix(p,nrow=n,ncol=n)
	P[1:m,] <- r
	P[,1:m] <- r
	P[1:m,1:m] <- q
	rg(n,P)
}
