rdpg.game <- function(x,y=NULL,n=100,d=2,directed=FALSE,use.x=FALSE)
{
	 if(missing(x)){
		x <- rdirichlet(n,rep(1,d+1))[,-1,drop=FALSE]
	 }
	 if(directed){
		if(use.x) {
		   y <- x
		} else if(is.null(y)){
			y <- rdirichlet(nrow(x),rep(1,ncol(x)+1))[,-1,drop=FALSE]
		}
	 }
	 if(directed){
		 if((nrow(x) != nrow(y)) || (ncol(x) != ncol(y))){
			 stop("x and y must have the same dimensions")
		 }
	 }
	 if(directed) {
	    p <- x %*% t(y)
	 } else {
		 p <- x %*% t(x)
	 }
	 p[p<0] <- 0
	 p[p>1] <- 1
	 n <- nrow(p)
	 A <- p>=matrix(runif(n*n),nrow=n,ncol=n)
	 if(!directed){
		 A[upper.tri(A)] <- t(A)[upper.tri(A)]
	 }
	 diag(A) <- 0
	 g <- graph.adjacency(A,mode=ifelse(directed,"directed","undirected"))
	 g <- set.graph.attribute(g,name="x",value=x)
	 if(directed){
		 g <- set.graph.attribute(g,name="y",value=y)
	 }
	 g
}

rdpg.estimate <- function(g,d=2)
{
   
    n <- vcount(g)
	 M <- get.adjacency(g)
    D <- M
    degs <- degree(g)
    diag(D) <- degs/(n-1)
	 if(is.directed(g)){
		 xx <- svd(D)
		 v <- xx$d[1:d]
		 s <- 1/sqrt(v)
		 s[v == 0] <- 0
		 X <- scale(xx$u[, 1:d,drop=FALSE], center = FALSE, scale = s)
		 Y <- scale(xx$v[, 1:d,drop=FALSE], center = FALSE, scale = s)
		 out <- list(x=X,y=Y,values = xx$d)
	 } else {
		 xx <- eigen(D, symmetric=TRUE)
		 v <- xx$values[1:d]
		 s <- 1/sqrt(v)
		 s[v == 0] <- 0
		 X <- scale(xx$vectors[, 1:d,drop=FALSE], center = FALSE, scale = s)
		 out <- list(x=X,values = xx$d)
	 }
	 out
}
