plotGraph <- function(g,n,x,...)
{
	if(missing(n))
		n <- Order(g)
	s <- Size(g)
   if(missing(x)){
	   theta <- seq(0,2*pi,length=n+1)[1:n]
		x <- cbind(cos(theta),sin(theta))
	}
	plot(x,...)
	segments(x[g[,1],1],x[g[,1],2],x[g[,2],1],x[g[,2],2])
	invisible(x)
}

plotAdjacency <- function(g,col=0:1,addLine=FALSE,...)
{
	X <- adjacencyMatrix(g)
	A <- X
	n <- Order(g)
	for(i in 1:n){
	   A[,i] <- X[,n-i+1]
	}

	image(A,axes=FALSE,col=col,...)
	box()
	if(addLine) abline(1,-1)
}

plotMFR <- function(m,icolors=gray((c(255,0:255))/255),
                      tcolors=2,zeros=FALSE,log=FALSE,cex=1,box=FALSE)
{
   nf <- layout(rbind(c(1,1,1),
	                   c(1,1,1),
	                   c(2,2,2),
							 c(2,2,2),
							 c(2,2,2),
							 c(2,2,2)))
	par(mar=c(2,4,4,2)+.1)
   plot(2:length(m$bettis),m$bettis[-1],
	     type="l",xlab="",ylab=expression(beta))
	M <- m$graded[-1,-1]
	X <- M
	n <- nrow(M)
	for(i in 1:n){
	   X[i,] <- M[n-i+1,]
	}
	Y <- rbind(X,rep(max(m$bettis)+1,length(m$bettis)-1),m$bettis[-1])
	if(log) Y <- log(Y)
	par(mar=c(2,4,4,2)+.1)
	image(1:ncol(Y),1:nrow(Y),t(Y),col=icolors,
	      xlab="",ylab="",axes=FALSE)
	if(tcolors>0){
		for(i in 1:(length(m$bettis)-1)){
			text(i,nrow(Y),label=m$bettis[i+1],col=tcolors,cex=cex)
		}
		for(i in 1:ncol(M)){
			for(j in 1:nrow(M)){
				if(M[j,i]!=0 || zeros)
					text(i,nrow(M)-j+1,labels=M[j,i],col=tcolors,cex=cex)
			}
		}
	}
	if(box) box()
}

plotScanMFR <- function(ms,icolors=gray((c(255,0:255))/255),col=gray(0.8),
                      tcolors=2,zeros=FALSE,log=FALSE,cex=1,box=FALSE)
{
   nf <- layout(rbind(c(1,1,1),
	                   c(1,1,1),
	                   c(2,2,2),
							 c(2,2,2),
							 c(2,2,2),
							 c(2,2,2)))
	par(mar=c(2,4,4,2)+.1)
	ymax <- max(unlist(lapply(ms,function(x)max(x$bettis))))
	m <- meanScanMFR(ms)
   plot(2:length(m$bettis),m$bettis[-1],ylim=c(0,ymax),
	     type="l",xlab="",ylab=expression(beta))
	for(i in 1:length(ms)){
		if(length(ms[[i]]$bettis)>1){
			lines(2:length(ms[[i]]$bettis),ms[[i]]$bettis[-1],col=col)
		}
	}
	lines(2:length(m$bettis),m$bettis[-1],col=1)
	M <- m$graded[-1,-1]
	n <- nrow(M)
	if(is.null(n)) {
	   n <- 1
		M <- matrix(M,nrow=1)
	}
	X <- M
	for(i in 1:n){
	   X[i,] <- M[n-i+1,]
	}
	Y <- rbind(X,rep(max(m$bettis)+1,length(m$bettis)-1),m$bettis[-1])
	if(log) Y <- log(Y)
	par(mar=c(2,4,4,2)+.1)
	image(1:ncol(Y),1:nrow(Y),t(Y),col=icolors,
	      xlab="",ylab="",axes=FALSE)
	if(tcolors>0){
		for(i in 1:(length(m$bettis)-1)){
			text(i,nrow(Y),label=m$bettis[i+1],col=tcolors,cex=cex)
		}
		for(i in 1:ncol(M)){
			for(j in 1:nrow(M)){
				if(M[j,i]!=0 || zeros)
					text(i,nrow(M)-j+1,labels=M[j,i],col=tcolors,cex=cex)
			}
		}
	}
	if(box) box()
}
