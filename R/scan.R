scanMFR1 <- function(g,use.max=TRUE,
                     linear.strand=FALSE,maxBetti=vcount(g),...)
{
   n <- vcount(g)
	out <- list(0)
	v <- V(g)
	nr <- 1
	nc <- 1
	punted <- 0
   for(i in v){
		N <- neighborhood(g,order=1,nodes=i)[[1]]
		if(length(N)==1){
			out[[i+1]] <- list(bettis=1,pd=0,reg=1,graded=matrix(1,nrow=1),punted=0)
			class(out[[i+1]]) <- c("mfr","exact")
		} else {
			h <- subgraph(g,N)
			a <- checkDB(h)
			if(is.null(a)){
				if(linear.strand){
					out[[i+1]] <- linearStrand(h,maxBetti=maxBetti)
				} else {
					out[[i+1]] <- MFRStar(subgraph(g,setdiff(N,i)),...)
				}
			} else {
				if(linear.strand){
					out[[i+1]] <- a[2,,drop=FALSE]
				} else {
					out[[i+1]] <- a
				}
			}
		}
		nr <- max(nrow(out[[i+1]]$graded),nr)
		nc <- max(ncol(out[[i+1]]$graded),nc)
		punted <- out[[i+1]]$punted+punted
	}
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

scanMFR <- function(g,method="size",by.vertex=FALSE,
						  linear.strand=FALSE,maxBetti=vcount(g),
                    ...)
{
	if(ecount(g)==0) stop("The graph is empty")
	N <- neighborhood(g,order=1)
	M <- c("size","order","maximum","minimum")
	meth <- pmatch(tolower(method),M)
	if(is.na(meth)){
		if(by.vertex){
		   ind <- which.max(do.call(method,args=list(graph=g)))
		} else {
			ind <- which.max(unlist(lapply(N,
		                  function(x) 
								    do.call(method,args=list(graph=subgraph(g,x))))))
		}
		sub <- setdiff(N[[ind]],ind-1)
	}
	a <- NULL
   if(meth==1){
	   ind <- which.max(unlist(lapply(N,function(x)ecount(subgraph(g,x)))))
		sub <- N[[ind]]
	} else if(meth==2){
	   ind <- which.max(unlist(lapply(N,length)))
		sub <- N[[ind]]
	} else if(meth==3){
		a <- scanMFR1(g,use.max=TRUE,
		              linear.strand=linear.strand,maxBetti=maxBetti,...)
	} else if(meth==4){
		a <- scanMFR1(g,use.max=FALSE,
		              linear.strand=linear.strand,maxBetti=maxBetti,...)
	}
	if(is.null(a)){
		h <- subgraph(g,sub)
		a <- checkDB(h)
		if(is.null(a)){
			if(linear.strand){
				a <- linearStrand(h,maxBetti=maxBetti)
			} else {
				h <- subgraph(g,setdiff(sub,ind-1))
				a <- MFRStar(h,...)
			}
		} else {
		   if(linear.strand){
			   b <- a$graded[2,1:(min(ncol(a$graded),maxBetti+1))]
				class(b) <- c("linear.strand","exact")
				a <- b
			}
		}
		a$method <- method
		a$center <- ind-1
		class(a) <- c("mfrScan","mfr")
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

