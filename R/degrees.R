degrees <- function(g)
{
	n <- Order(g)
   d <- rep(0,n)
	for(i in 1:n){
	   d[i] <- d[i]+sum(g[,1]==i | g[,2]==i)
	}
	d
}
