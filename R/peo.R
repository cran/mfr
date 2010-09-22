is.complete <- function(g)
{
   Size(g) == choose(Order(g),2)
}

is.cycle <- function(g)
{
   is.connected(removeIsolates(g)) && all(degrees(g)==2)
}

simplicial.vertex <- function(g,n)
{
	if(missing(n))
		n <- Order(g)
	if(n==1) return(1)
	for(i in 1:n){
	   N <- neighborhood(g,i,open=FALSE)
		if(length(N)>1 && is.complete(subgraph(g,N))) return(i)
	}
	return(NULL)
}

remove.vertices <- function(g,v)
{
   E <- NULL
	for(i in 1:Size(g)){
	   if(!any(v %in% g[i,])){
		   E <- rbind(E,g[i,])
		}
	}
	E
}

isolates <- function(g,n)
{
	if(missing(n)) n <- Order(g)
   I <- setdiff(1:n,unique(c(g)))
	if(length(I)==0) return(NULL)
	I
}

# perfect elimination ordering, slow version
peo <- function(g)
{
	n <- Order(g)
   ordering <- isolates(g)
	G <- g
	v <- simplicial.vertex(G,n=n)
	if(is.null(v)) return(ordering)
	ordering <- c(ordering,v)
	G <- remove.vertices(g,ordering)
	for(i in (n-1):2){
		v <- simplicial.vertex(G,n=n)
		if(is.null(v)) {
			ordering <- c(ordering,setdiff(isolates(G,n=n),ordering))
			return(ordering)
		}
		ordering <- c(ordering,v)
		G <- remove.vertices(g,ordering)
	}
	ordering <- c(ordering,setdiff(isolates(G,n=n),ordering))
	ordering
}

is.chordal <- function(g)
{
   a <- peo(g)
	length(a)==Order(g)
}
