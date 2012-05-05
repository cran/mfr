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

count.squares <- function(g,induced=TRUE)
{
	if(vcount(g)<4) return(0)
	a <- 0
	for(i in 1:(vcount(g)-1)){
		ni <- setdiff(neighborhood(g,order=1,nodes=i-1)[[1]],i-1)
		if(length(ni)>1){
			for(j in (i+1):vcount(g)){
				nj <- setdiff(neighborhood(g,order=1,nodes=j-1)[[1]],j-1)
				if(induced){
					if(length(intersect(ni,nj))>1){
						x <- combn(intersect(ni,nj),2)
						for(k in 1:ncol(x)){
						   if(ecount(subgraph(g,c(x[,k],i-1,j-1)))==4){
							   a <- a+1
							}
						}
					}
				} else {
					a <- a+choose(length(intersect(ni,nj)),2)
				}
			}
		}
	}
	a/2
}

count.bipartite <- function(g,m,n)
{
   a <- min(n,m)
	n <- max(n,m)
	m <- a
	d <- degree(g)
	nodes <- which(d>=m)
	if(length(nodes)==0) return(0)
	h1 <- subgraph(g,nodes-1)
	vc <- 0
	while(vc != vcount(h1)){
	   vc <- vcount(h1)
		d <- degree(h1)
		nodes <- which(d>=m)
		if(length(nodes)==0) return(0)
		h1 <- subgraph(h1,nodes-1)
	}

	if(vc<(n+m)) return(0)
	if(ecount(h1)<(n*m)) return(0)

	N <- neighborhood(h1,order=1)
	x <- NULL
	a <- combn(vcount(h1),m)-1
	b <- apply(a,2,function(x) ecount(subgraph(h1,x))==0)
	if(any(b)){
		a1 <- a[,b,drop=FALSE]
		for(i in 1:ncol(a1)){
			n1 <- a1[,i]
			n2 <- setdiff(unlist(N[n1+1]),n1)
			if(length(n2)>=n){
				a <- combn(n2+1,n)-1
				b <- apply(a,2,function(x) ecount(subgraph(h1,x))==0)
				if(any(b)){
				   a2 <- a[,b,drop=FALSE]
					for(j in 1:ncol(a2)){
						nodes <- sort(unique(c(n1,a2[,j])))
						if(length(nodes)==(n+m)){
							h2 <- subgraph(h1,nodes)
							if(ecount(h2)==(n*m)){
							   x <- rbind(x,nodes)
							}
						}
					}
				}
			}
		}
	}

	if(is.null(x)) return(0)
	x <- unique(x)
	nrow(x)
}

