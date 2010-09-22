complement <- function(g,n=Order(g))
{
#this is a very inefficient way to do this
	h <- completeGraph(n)
	for(i in 1:nrow(g)){
	   h <- delete.edge(h,g[i,])
	}
	h
}
