
rchordal <- function(n,size=n-1,g,from.tree=TRUE,from.ring=FALSE,add=TRUE,
                     random.size=FALSE)
{
	if(!missing(g)) {
		n <- vcount(g)
	}
	if(missing(n)){
		stop("must provide n")
	}
	if(n==1) return(graph.empty(n))
	if(n==2) return(graph(c(0,1),directed=FALSE))
	if(random.size){
	   size <- sample((n-1):choose(n,2),1)
	}
	if(size<(n-1)){
	   stop("size must be at least n-1")
	}
	if(size==choose(n,2)) return(graph.full(n))
	if(missing(g)){
		if(from.tree){
			g <- rtree(n)
			if(size==(n-1)) return(g)
		} else if(from.ring){
			g <- graph.ring(n=n,directed=FALSE,mutual=FALSE,circular=TRUE)
			return(rchordal.add(g))
		} else {
			g <- graph.empty(n=n,directed=FALSE)
		}
	} else {
		if(!is.chordal(g)){
		   if(add) return(rchordal.add(g))
		   else return(rchordal.subtract(g))
		} else if(ecount(g)>size){
			stop("g must be have no more than size edges")
		} else if(ecount(g)==size){
		   return(g)
		}
	}
	h <- graph.complementer(g)
	edges <- get.edgelist(h)
	edges <- edges[sample(nrow(edges),nrow(edges),replace=FALSE),]
	while(ecount(g)<size){
		for(edge in 1:nrow(edges)){
			if(is.chordal(add.edges(g,edges[edge,]))){
				g <- add.edges(g,edges[edge,])
				if(ecount(g)==size){
					break
				}
			}
		}
	}
	g
}

rchordal.add <- function(g)
{
	if(is.chordal(g)) return(g)
	edges <- get.edgelist(graph.complementer(g))
	while(!is.chordal(g)){
		ind <- sample(nrow(edges),1)
		g <- add.edges(g,edges[ind,])
		edges <- edges[-ind,]
	}
	g
}

rchordal.subtract <- function(g)
{
	if(is.chordal(g)) return(g)
	while(!is.chordal(g)){
		edge <- sample(ecount(g),1)-1
		g <- delete.edges(g,edge)
	}
	g
}

