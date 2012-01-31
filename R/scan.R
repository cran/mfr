scanMFR1 <- function(g,use.max=TRUE,...)
{
   n <- vcount(g)
	out <- list(0)
	v <- V(g)
	nr <- 1
	nc <- 1
	punted <- 0
   for(i in 1:n){
		if(i %in% v){
			N <- neighborhood(g,order=1,nodes=i)[[1]]
			out[[i]] <- mfr(subgraph(g,N),...)
		}
		else{
		   out[[i]] <- list(bettis=1,graded=matrix(1,nrow=1),pd=0,reg=1,punted=0)
		}
		nr <- max(nrow(out[[i]]$graded),nr)
		nc <- max(ncol(out[[i]]$graded),nc)
		punted <- out[[i]]$punted+punted
	}
	punted <- 0
	pd <- nc-1
	reg <- nr
	bettis <- rep(NA,nc)
	for(i in 1:nc){
		for(j in 1:length(out)){
		   b <- out[[j]]$bettis
			if(i<=length(b)){
				if(use.max){
				   bettis[i] <- max(c(bettis[i],b[i]),na.rm=TRUE)
				} else {
				   bettis[i] <- min(c(bettis[i],b[i]),na.rm=TRUE)
				}
			} else if(!use.max){
			   bettis[i] <- 0
			}
		}
	}
	M <- matrix(NA,nrow=nr,ncol=nc)
	for(i in 1:nr){
		for(j in 1:nc){
			for(k in 1:length(out)){
			   g <- out[[k]]$graded
				if((i<=nrow(g)) && (j<=ncol(g))){
					if(use.max){
						M[i,j] <- max(c(M[i,j],g[i,j]),na.rm=TRUE)
					} else {
						M[i,j] <- min(c(M[i,j],g[i,j]),na.rm=TRUE)
					}
				} else if(!use.max){
				   M[i,j] <- 0
				}
			}
		}
	}
	a <- list(bettis=bettis,graded=M,reg=reg,pd=pd,punted=punted,
	          method=ifelse(use.max,"Maximum","Minimum"))
	class(a) <- c("mfrScan","mfr")
	a
}

scanMFR <- function(g,method="size",...)
{
	N <- neighborhood(g,order=1)
	M <- c("size","order","maximum","minimum")
	meth <- pmatch(tolower(method),M)
	if(is.na(meth)){
	   stop("invalid method passed to scanMFR")
	}
   if(meth==1){
	   ind <- which.max(unlist(lapply(N,function(x)ecount(subgraph(g,x)))))
		h <- subgraph(g,N[[ind]])
		a <- mfr(h,...)
		a$method <- "By Size"
		a$center <- ind-1
		class(a) <- c("mfrScan","mfr")
	} else if(meth==2){
	   ind <- which.max(unlist(lapply(N,length)))
		h <- subgraph(g,N[[ind]])
		a <- mfr(h,...)
		a$method <- "By Order"
		a$center <- ind-1
		class(a) <- c("mfrScan","mfr")
	} else if(meth==3){
		a <- scanMFR1(g,use.max=TRUE,...)
	} else if(meth==4){
		a <- scanMFR1(g,use.max=FALSE,...)
	}
	a
}

print.mfrScan <- function(x,...)
{
   cat("Scan method:",x$method,"\n")
	cat("Minimal Free Resolution:\n")
   cat("Total betti numbers:\n")
	for(i in 1:length(x$bettis)){
	   cat("\t",x$bettis[i])
	}
	cat("\n\nGraded:")
	for(i in 1:nrow(x$graded)){
		for(j in 1:ncol(x$graded)){
		   cat("\t",x$graded[i,j])
		}
		cat("\n")
	}
	cat("\n")
	if(x$punted>0) cat("Called Singular",x$punted,"times\n")
}

