MFRbwg <- function(spokes)
{
	# uses Johannsen's cycle splitting theorem
   #a <- mfr(cycleGraph(2*spokes))
	#b <- mfr(starGraph(spokes+1))
	a <- MFRCycle(2*spokes)
	b <- MFRStar(spokes+1)
	pd <- a$pd+1
	M <- cbind(a$graded,rep(0,nrow(a$graded)))
	N <- matrix(M[-1,-1],ncol=ncol(M)-1)
	N[,2:ncol(N)] <- N[,2:ncol(N)]+a$graded[-1,-1]
	X <- matrix(b$graded[-1,-1],nrow=nrow(b$graded)-1)
	N[1:nrow(X),1:ncol(X)] <- N[1:nrow(X),1:ncol(X)] + X
	M <- rbind(c(1,rep(0,ncol(N))),cbind(rep(0,nrow(N)),N))
	B <- c(0,a$bettis[-1])+c(a$bettis[-1],0)
	B[1:(length(b$bettis)-1)] <- B[1:(length(b$bettis)-1)]+b$bettis[-1]
	B <- c(1,B)
	m <- list(bettis=B,graded=M,reg=nrow(M),pd=pd,punted=0)
	class(m) <- c("mfr","exact")
	m
}

MFRComplete <- function(n)
{
	if(n<=1) stop("n must be at least 2")
	graded <- rep(0,n*n)
	retval <- .C("MFRComplete",
					 N = as.integer(n),
					 graded = as.double(graded),
					 pd = integer(1),
					 reg = integer(1),
					 PACKAGE="mfr")
	graded <- matrix(retval$graded[1:(retval$reg*(retval$pd+1))],
						  byrow=TRUE,
	                 nrow=retval$reg,ncol=retval$pd+1)
	bettis <- apply(graded,2,sum)
	graded <- graded[,bettis>0]
	bettis <- bettis[bettis>0]
	m <- list(bettis=bettis,graded=graded,reg=nrow(graded),pd=ncol(graded)-1,
	      punted=0)
	class(m) <- c("mfr","exact")
	m
}

MFRCompleteBipartite <- function(n,m)
{
	if(n<1) stop("n must be at least 1")
	if(m<1) stop("m must be at least 1")
	graded <- rep(0,(n+m)*(n+m))
	retval <- .C("MFRCompleteBipartite",
					 N = as.integer(n),
					 M = as.integer(m),
					 graded = as.double(graded),
					 pd = integer(1),
					 reg = integer(1),
					 PACKAGE="mfr")
	graded <- matrix(retval$graded[1:(retval$reg*(retval$pd+1))],
						  byrow=TRUE,
	                 nrow=retval$reg,ncol=retval$pd+1)
	bettis <- apply(graded,2,sum)
	graded <- graded[,bettis>0]
	bettis <- bettis[bettis>0]
	m <- list(bettis=bettis,graded=graded,reg=nrow(graded),pd=ncol(graded)-1,
	      punted=0)
	class(m) <- c("mfr","exact")
	m
}

MFRCycle <- function(n)
{
	if(n<=1) stop("n must be at least 2")
	graded <- rep(0,n*n)
	retval <- .C("MFRCycle",
					 N = as.integer(n),
					 graded = as.double(graded),
					 pd = integer(1),
					 reg = integer(1),
					 PACKAGE="mfr")
	graded <- matrix(retval$graded[1:(retval$reg*(retval$pd+1))],
						  byrow=TRUE,
	                 nrow=retval$reg,ncol=retval$pd+1)
	bettis <- apply(graded,2,sum)
	graded <- graded[,bettis>0]
	bettis <- bettis[bettis>0]
	m <- list(bettis=bettis,graded=graded,reg=nrow(graded),pd=ncol(graded)-1,
	      punted=0)
	class(m) <- c("mfr","exact")
	m
}

MFRStar <- function(n,...)
{
	if(is.igraph(n)){
		if(ecount(n)==0) return(MFRStar(vcount(n)+1))
		if(ecount(n)==(vcount(n)-1)) return(MFRStar(vcount(n)))
	   m1 <- mfr(n,...)
		graded <- m1$graded
		N <- vcount(n)
		y <- sapply(1:(N+1),function(a)choose(N,a))
		x <- matrix(0,nrow=nrow(graded),ncol=length(y)+1)
		x[1,1] <- 1
		g1 <- matrix(0,nrow=nrow(graded),ncol=max(length(y),ncol(graded)))
		g2 <- matrix(0,nrow=nrow(graded),ncol=max(length(y),ncol(graded)))
		g1[,1:ncol(graded)] <- graded
		if(ncol(graded)>0){
			g2[,1:(ncol(graded)-1)] <- graded[,-1]
		}
		x[2:nrow(x),2:(length(y)+1)] <- g1[2:nrow(x),] + g2[2:nrow(x),]
		x[2,2:(length(y)+1)] <- x[2,2:(length(y)+1)]+y
		a <- apply(x,2,sum)
		graded <- x[,a>0]
	} else {
		if(n<=1) stop("n must be at least 2")
		d <- n-1
		graded <- rep(0,n*n)
		retval <- .C("MFRStar",
						 D = as.integer(d),
						 graded = as.double(graded),
						 pd = integer(1),
						 reg = integer(1),
						 PACKAGE="mfr")
		graded <- matrix(retval$graded[1:(retval$reg*(retval$pd+1))],
							  byrow=TRUE,
							  nrow=retval$reg,ncol=retval$pd+1)
	}
	bettis <- apply(graded,2,sum)
	graded <- graded[,bettis>0]
	bettis <- bettis[bettis>0]
	m <- list(bettis=bettis,graded=graded,reg=nrow(graded),pd=ncol(graded)-1,
			punted=0)
	class(m) <- c("mfr","exact")
	m
}

MFRPath <- function(n)
{
	if(n<=1) stop("n must be at least 2")
	graded <- rep(0,n*n)
	retval <- .C("MFRPath",
					 N = as.integer(n),
					 graded = as.double(graded),
					 pd = integer(1),
					 reg = integer(1),
					 PACKAGE="mfr")
	graded <- matrix(retval$graded[1:(retval$reg*(retval$pd+1))],
						  byrow=TRUE,
	                 nrow=retval$reg,ncol=retval$pd+1)
	bettis <- apply(graded,2,sum)
	graded <- graded[,bettis>0]
	bettis <- bettis[bettis>0]
	m <- list(bettis=bettis,graded=graded,reg=nrow(graded),pd=ncol(graded)-1,
	      punted=0)
	class(m) <- c("mfr","exact")
	m
}

MFRWheel <- function(n)
{
	if(n<2) stop("n must be at least 1")
	if((n %% 2) == 0) stop("n must be odd. Use mfr for n even.")
	k <- (n-1)/2
	graded <- matrix(0,nrow=n+1,ncol=n+1)
	x <- MFRbwg(k)
	graded[1:nrow(x$graded),1:ncol(x$graded)] <- x$graded
	#graded <- graded[-1,-1]
	for(i in 2:n){
		for(a in k:(n-2)){
		   graded[2,i] <- graded[2,i]+choose(a,i-2)
		}
	}
	bettis <- apply(graded,2,sum)
	graded <- graded[,bettis>0]
	bettis <- bettis[bettis>0]
	z <- apply(graded,1,max)
	graded <- graded[which(z>0),]
	m <- list(bettis=bettis,graded=graded,reg=nrow(graded),pd=ncol(graded)-1,
	      punted=0)
	class(m) <- c("mfr","exact")
	m
}

