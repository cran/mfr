Star <- function(x)
{
   if(is.igraph(x)){
		if(vcount(x)==0) return(graph.empty(1,directed=FALSE))
	   g <- add.vertices(x,1)
		edges <- rbind(0:(vcount(x)-1),vcount(x))
		g <- add.edges(g,edges)
	} else if(is.numeric(x)){
	   g <- graph.star(x+1,mode="undirected")
	} else {
	   stop("x must be a graph or a positive integer")
	}
	g
}

graph.wheel <- function(n)
{
	Star(graph.ring(n-1))
}

graph.bwg <- function(n)
{
	if(n<=3) stop("n must be at least 5")
   if((n %% 2) == 0) stop("n must be odd")
	g <- graph.wheel(n)
	edges <- seq(n-1,ecount(g)-1,by=2)
	delete.edges(g,edges)
}
