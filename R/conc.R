
layout.concentric.circles <- function(graph,params)
{
	if(missing(params)){
	   nodes <- 0
	} else {
	   nodes <- params$nodes
	}
	n <- vcount(graph)
	x <- matrix(0,nrow=n,ncol=2)
	if(length(nodes)>1){
		x[nodes+1,] <- layout.circle(subgraph(graph,nodes))
	}
	for(i in 2:n){
		nbd <- unique(unlist(neighborhood(graph,order=1,nodes=nodes)))
		if(length(nbd)==length(nodes)){
			break
		} else {
			z <- setdiff(nbd,nodes)
			if(length(z)>0){
				x[z+1,] <- i*layout.circle(subgraph(graph,z))
			}
			nodes <- nbd
		}
	}
	if(length(nodes)<n){
	   z <- setdiff(V(graph),nodes)
		x[z+1,] <- i*layout.circle(subgraph(graph,z))
	}
	x
}

