is.chordal.comp <- function(g)
{
   is.chordal(graph.complementer(g))
}

chordal.comp.mfr <- function(g,maxBetti=vcount(g))
{
   graded <- matrix(0,nrow=2,ncol=max(3,min(maxBetti,vcount(g))))
	graded[1,1] <- 1
	graded[2,2] <- ecount(g)
	f <- function(x) {
	    (-1+clusters(graph.complementer(subgraph(g,x-1)))$no)
	}
	for(k in 3:ncol(graded)){
		graded[2,k] <- sum(combn(vcount(g),k,FUN=f))
		if(graded[2,k]==0) {
			graded <- graded[,1:(k-1)]
			break
		}
	}
	m <- list(bettis=apply(graded,2,sum),graded=graded,reg=2,
	          pd=ncol(graded)-1,punted=0)
   class(m) <- c("mfr","exact")
	m
}

chordalComp1 <- function(z)
{
	s <- length(z)/2
	e1 <- z[(1:s)]
	e2 <- z[-(1:s)]
   g <- graph(rbind(e1,e2),directed=FALSE)
	a <- 0
	if(is.chordal.comp(g)){
		m <- chordal.comp.mfr(g)
		a <- as.integer(c(m$pd,m$reg,c(t(m$graded))))
	}
	return(as.integer(a))
}

