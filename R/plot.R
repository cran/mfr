plot.mfr <- function(x,...,
  icolors=gray((c(255,0:255))/255),
                     tcolors=2,logMFR=FALSE,zeros=FALSE)
{
   nf <- layout(rbind(c(1,1,1),
	                   c(1,1,1),
	                   c(2,2,2),
							 c(2,2,2),
							 c(2,2,2),
							 c(2,2,2)))
	par(mar=c(2,4,4,2)+.1)
	passed.args <- list(...)
	if(is.null(passed.args$xlab)) {
		xlab <- ""
	} else {
	   xlab <- passed.args$xlab
	}
   plot(2:length(x$bettis),x$bettis[-1],
	     type="l",xlab=xlab,ylab=expression(beta),
		  ...)
	M <- x$graded[-1,-1]
	X <- M
	n <- nrow(M)
	for(i in 1:n){
	   X[i,] <- M[n-i+1,]
	}
	Y <- rbind(X,rep(max(x$bettis)+1,length(x$bettis)-1),x$bettis[-1])
	if(logMFR) Y <- log(Y)
	par(mar=c(2,4,4,2)+.1)
	image(1:ncol(Y),1:nrow(Y),t(Y),col=icolors,
	      xlab="",ylab="",axes=FALSE)
	if(tcolors>0){
		if(is.null(passed.args$cex)){
		   cex <- 1
		} else {
		   cex <- passed.args$cex
		}
		for(i in 1:(length(x$bettis)-1)){
			text(i,nrow(Y),label=x$bettis[i+1],col=tcolors,cex=cex)
		}
		for(i in 1:ncol(M)){
			for(j in 1:nrow(M)){
				if(M[j,i]!=0 || zeros)
					text(i,nrow(M)-j+1,labels=M[j,i],col=tcolors,cex=cex)
			}
		}
	}
	if(!is.null(passed.args$box)){
		if(passed.args$box) box()
	}
}

