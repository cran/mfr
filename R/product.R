product.game <- function(g,h,p,n)
{
	use.p <- TRUE
   if(missing(p)){
	   if(missing(n)){
		   stop("must provide p or n")
		}
		use.p <- FALSE
	}


	n1 <- vcount(g)
	n2 <- vcount(h)

	if(!use.p && (n1*n2<n)){
	   stop(paste("cannot add more than",n1*n2,"edges between these graphs"))
	}

	k <- graph.disjoint.union(g,h)

	if(use.p){
      n <- rbinom(1,n1*n2-1,p)+1
	}
	x <- 0
	edges <- unique(cbind(sample(n1,n,replace=TRUE),
	                      sample(n2,n,replace=TRUE)+n1))
	while(nrow(edges)<n){
		m <- nrow(edges)
		edges <- unique(rbind(edges,cbind(sample(n1,n-m,replace=TRUE),
		                                  sample(n2,n-m,replace=TRUE)+n1)))
	}
	add.edges(k,t(edges)-1)
}
