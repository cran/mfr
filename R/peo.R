is.complete <- function(g)
{
   ecount(g) == choose(vcount(g),2)
}

is.cycle <- function(g)
{
   is.connected(removeIsolates(g)) && all(degree(g)==2)
}

simplicial.vertex <- function(g,n)
{
	if(missing(n))
		n <- vcount(g)
	if(n==1) return(1)
	for(i in V(g)){
	   N <- neighborhood(g,order=1,nodes=i)[[1]]
		if(length(N)>1 && is.complete(subgraph(g,N))) return(i)
	}
	return(NULL)
}

remove.adjacent.edges <- function(g,v)
{
   edges <- get.edgelist(g)
	for(u in v){
	   a <- which(edges[,1]==u)
	   b <- which(edges[,2]==u)
		i <- union(a,b)
		if(length(i)==nrow(edges)) return(graph.empty(vcount(g)))
		if(length(i)>0){
		   edges <- edges[-i,]
			if(!is.matrix(edges)) edges <- matrix(edges,ncol=2)
		}
	}
	graph(t(edges),n=vcount(g),directed=FALSE)
}

# perfect elimination ordering, slow version
peo <- function(g)
{
	n <- vcount(g)
   ordering <- isolates(g)
	G <- g
	v <- simplicial.vertex(G,n=n)
	if(is.null(v)) return(ordering)
	ordering <- c(ordering,v)
	G <- remove.adjacent.edges(g,ordering)
	for(i in (n-1):2){
		v <- simplicial.vertex(G,n=n)
		if(is.null(v)) {
			return(ordering)
		}
		ordering <- c(ordering,v)
		G <- remove.adjacent.edges(g,ordering)
		if(ecount(G)==0) break
	}
	c(ordering,setdiff(0:(n-1),ordering))
}

is.chordal <- function(g)
{
   a <- peo(g)
	length(a)==vcount(g)
}
