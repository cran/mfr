
rchordal <- function(n,size=n-1)
{
	if(missing(n)){
		stop("must provide n")
	}
	if(size<(n-1)){
	   stop("size must be at least n-1")
	}
	if(size==choose(n,2)) return(completeGraph(n))
	g <- rtree(n)
	if(size==(n-1)) return(g)
	s <- Size(g)
	h <- complement(g,n)
	indices <- 1:nrow(h)
	while(s<size){
		edges <- sample(indices,length(indices),replace=FALSE)
		for(edge in edges){
			if(is.chordal(rbind(g,h[edge,]))){
				g <- rbind(g,h[edge,])
				indices <- setdiff(indices,edge)
				s <- s+1
				break
			}
		}
	}
	g
}

rchordal.add <- function(g)
{
	if(is.chordal(g)) return(g)
	h <- complement(g)
	while(!is.chordal(g)){
		ind <- sample(nrow(h),1)
		g <- rbind(g,h[ind,])
		h <- h[-ind,]
	}
	g
}

rchordal.subtract <- function(g)
{
	if(is.chordal(g)) return(g)
	while(!is.chordal(g)){
		ind <- sample(nrow(g),1)
		g <- g[-ind,]
	}
	g
}

chordal.add <- function(g)
{
	if(is.chordal(g)) return(g)
	h <- complement(g)
	while(!is.chordal(g)){
		for(i in 1:nrow(h)){
		   k <- rbind(g,h[i,])
			if(is.chordal(k)) return(k)
		}
		ind <- sample(nrow(h),1)
		g <- rbind(g,h[ind,])
		h <- h[-ind,]
	}
	g
}

chordal.subtract <- function(g)
{
	if(is.chordal(g)) return(g)
	while(!is.chordal(g)){
		for(i in 1:nrow(g)){
		   h <- g[-i,]
			if(is.chordal(h)) return(h)
		}
		ind <- sample(nrow(g),1)
		g <- g[-ind,]
	}
	g
}

chordal.sandwich <- function(g)
{
   list(lower=chordal.subtract(g),upper=chordal.add(g))
}

