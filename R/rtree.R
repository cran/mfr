
# check to see if there is a cycle containing v
has.cycle <- function(g,v)
{
	N <- setdiff(neighborhood(g,order=1,nodes=v)[[1]],v)
	if(length(N)==0) return(FALSE)
	for(u in N){
	   h <- delete.edges(g,E(g,P=c(u,v)))
		comp <- clusters(h)$membership
		M <- which(comp==comp[u+1])-1
		if(v %in% M) return(TRUE)
	}
	FALSE
}

rtree <- function(n)
{
   g <- graph(sample(n,2)-1,directed=FALSE)
	s <- 1
	while(s<(n-1)){
	   edge <- sort(sample(n,2)-1)
		edges <- get.edgelist(g)
		a <- !(any((edges[,1]==edge[1]) & (edges[,2]==edge[2])) ||
		       any((edges[,1]==edge[2]) & (edges[,2]==edge[1])))
		if(a){
		   h <- graph(t(rbind(edges,edge)),directed=FALSE)
			if(!has.cycle(h,edge[1])){
			   g <- h
				s <- s+1
			}
		}
	}
	g
}
