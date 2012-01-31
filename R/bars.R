count.bars <- function(g)
{
   count <- 0
	if(ecount(g)<2) return(0)
	edges <- get.edgelist(g)
	for(i in 1:(nrow(edges)-1)){
	   for(j in (i+1):nrow(edges)){
			a <- union(edges[i,],edges[j,])
			if(length(a)==4){
				h <- subgraph(g,a)
				if(ecount(h)==2){
				   if(max(degree(h)==1)){
					   count <- count+1
					}
				}
			}
		}
	}
	count
}

count.angles <- function(g)
{
   nt <- length(cliques(g,3,3))
	nbhds <- neighborhood(g,order=1)
	cnt <- unlist(lapply(nbhds,function(x) choose(length(x)-1,2)))
	sum(cnt)-3*nt
}

count.triangles <- function(g) {
   length(cliques(g,3,3))
}
