
linearStrand1 <- function(g,maxBetti=vcount(g))
{
	x <- rep(0,vcount(g))
	d <- degree(g)
   for(i in 0:min(maxBetti,vcount(g))) {
		x[i+1] <- sum(sapply(d,choose,i+1))-length(cliques(g,i+2,i+2))
		if(x[i+1]==0) break
	}
	a <- matrix(c(0,x[x>0]),nrow=1)
	a <- list(graded=cbind(c(1,0),rbind(rep(0,ncol(a)),a)))
	class(a) <- c("linear.strand","exact")
	a
}

linearStrand2 <- function(g,maxBetti=vcount(g))
{
	n <- vcount(g)
	x <- rep(0,n)
	h <- graph.complementer(g)
   for(i in 1:min(maxBetti,(n-1))) {
		S <- combn(n,i+1)-1
		for(j in 1:ncol(S)){
		   k <- no.clusters(subgraph(h,S[,j]))-1
			x[i+1] <- x[i+1] + k
		}
		if(x[i+1]==0) break
	}
	a <- matrix(c(0,x[x>0]),nrow=1)
	a <- list(graded=cbind(c(1,0),rbind(rep(0,ncol(a)),a)))
	class(a) <- c("linear.strand","exact")
	a
}

linearStrandLower <- function(g,maxBetti=vcount(g))
{
	x <- rep(0,vcount(g))
	d <- degree(g)
	x[1] <- ecount(g)
	if(maxBetti==1) return(x[1])
	x[2] <- count.angles(g)+2*count.triangles(g)
	if(maxBetti==2) return(x[1:2])
	n <- vcount(g)
	A <- matrix(NA,nrow=100,ncol=100)
   for(i in 2:min(n,maxBetti)) {
		x[i+1] <- sum(sapply(d,choose,i+1))-length(cliques(g,i+2,i+2))
		j <- floor((i+2)/2)
		for(a in 2:j){
			z <- max(c(a,i+2-a))
			if(z>nrow(A)){
			   B <- matrix(NA,nrow=z,ncol=z)
				B[1:nrow(A),1:ncol(A)] <- A
				A <- B
			}
			if(is.na(A[a,i+2-a])){
			   b <- count.bipartite(g,a,i+2-a)
				A[a,i+2-a] <- b
				A[i+2-a,a] <- b
			} else {
			   b <- A[a,i+2-a]
			}
			x[i+1] <- x[i+1]+b
		}
		if(x[i+1]==0) break
	}
	a <- matrix(c(0,x[x>0]),nrow=1)
	a <- list(graded=cbind(c(1,0),rbind(rep(0,ncol(a)),a)))
	class(a) <- c("linear.strand","lower.bound")
	a
}

linearStrand <- function(g,maxBetti=vcount(g),exact=FALSE)
{
	if(ecount(g)==0) return(0)
	if(is.chordal(graph.complementer(g))){
	   a <- mfr(g)
		class(a) <- c("linear.strand","exact")
	   return(a)
	}
   if(count.squares(g)>0) {
		if(exact) {
		   a <- linearStrand2(g,maxBetti=maxBetti)
		} else {
			a <- linearStrandLower(g,maxBetti=maxBetti)
		}
	} else {
		a <- linearStrand1(g,maxBetti=maxBetti)
	}
	a
}

betti2 <- function(g) ecount(g)

betti3 <- function(g)
{
   x <- rep(0,2)
	x[1] <- count.angles(g)+2*count.triangles(g)
	x[2] <- count.bars(g)
	x
}

print.linear.strand <- function(x,...)
{
	cat("Linear Strand of a Minimal Free Resolution:\n")
	cat(x$graded,"\n")
	if(inherits(x,"exact")) cat("Betti numbers are exact\n")
	if(inherits(x,"lower.bound")) cat("Betti numbers are a lower bound\n")
}

