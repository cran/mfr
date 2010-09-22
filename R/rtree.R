
# check to see if there is a cycle containing v
has.cycle <- function(g,v)
{
	N <- neighborhood(g,v,open=TRUE)
	n <- Order(g)
	for(u in N){
	   h <- delete.edge(g,c(u,v))
		if(v %in% unique(c(h))){
			comp <- components(h)
			M <- which(comp==comp[u])
			if(v %in% M) return(TRUE)
		}
	}
	FALSE
}

rtree <- function(n)
{
   g <- matrix(sort(sample(1:n,2,replace=FALSE)),ncol=2)
	s <- 1
	while(s<(n-1)){
	   edge <- sort(sample(n,2))
		a <- !any((g[,1]==edge[1]) & (g[,2]==edge[2]) )
		if(a){
		   h <- rbind(g,edge)
			if(!has.cycle(h,edge[1])){
			   g <- h
				s <- s+1
			}
		}
	}
	rownames(g) <- NULL
	g
}
