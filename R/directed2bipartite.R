directed2bipartite <- function(g,add.internal=TRUE,remove.isolates=TRUE)
{
   if(!is.directed(g)) return(g)
	if(remove.isolates)
		g <- removeIsolates(g)
	edges <- get.edgelist(g)
	n <- vcount(g)
	edges <- cbind(edges[,1],edges[,2]+n)
	if(add.internal){
		edges <- rbind(edges,cbind(0:(n-1),(0:(n-1))+n))
	}
	graph(t(edges),directed=FALSE)
}

layoutD2B <- function(g,layout=get.graph.attribute(g,name="layout"),prop=1.1,
                      twiddle=FALSE,as.bipartite=FALSE)
{
	if(as.bipartite){
	   n <- vcount(g)
		layout <- cbind(rep(0:1,each=n),rep(1:n,2))
	} else {
		if(prop<1) prop <- 1.1
		if(is.null(layout)) {
			layout=layout.circle(g)
		} else {
			layout <- scale(layout)
		}
		layout <- rbind(layout,
		                layout*prop+twiddle*runif(2*nrow(layout),0,prop-1))
	}
	layout
}
