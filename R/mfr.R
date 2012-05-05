combineGraded <- function(gra,grb)
{
	N <- nrow(gra)+nrow(grb)
	M <- ncol(gra)+ncol(grb)
	G <- matrix(0,nrow=N,ncol=M)
	for(p in 1:nrow(gra)){
		for(q in 1:nrow(grb)){
			for(r in 1:ncol(gra)){
				for(s in 1:ncol(grb)){
					G[p+q,r+s] <- G[p+q,r+s]+gra[p,r]*grb[q,s]
				}
			}
		}
	}
	G[2:N,2:M]
}

mfr1 <- function(g,verbose=TRUE,nocode=FALSE,atrandom=FALSE,quiet=FALSE,
                 check.database=TRUE)
{
	if(check.database){
	   a <- checkDB(g)
		if(!is.null(a)) return(a)
	}

   n <- vcount(g)
	s <- ecount(g)

	if(s==0) {
	   return(list(bettis=1,pd=0,reg=1,graded=matrix(1,nrow=1),punted=0))
	} else if((vcount(g)>4) && is.chordal.comp(g)){
	   return(chordal.comp.mfr(g))
	}

	graded <- rep(0,n*n)

	tempname <- tempfile(paste("singular",n,s,"MFR",sep="_"))
	if(verbose){
	   cat("Graph = (",n,",",s,")\n",sep="")
		cat("tempname =",tempname,"\n")
	}
	edges <- get.edgelist(g)
	punted <- 0
	retval <- .C("mfr",
					 edges1 = as.integer(edges[,1]),
					 edges2 = as.integer(edges[,2]),
					 N = as.integer(n),
					 S = as.integer(s),
					 NC = integer(1),
					 V = as.integer(verbose),
					 graded = as.double(graded),
					 pd = integer(1),
					 reg = integer(1),
					 punted = as.integer(punted),
					 dontPunt=as.integer(nocode),
					 atrandom=as.integer(atrandom),
					 tempname=as.character(tempname),
					 quiet=as.integer(quiet),
					 checkdb=as.integer(check.database),
					 PACKAGE="mfr")
	 if(retval$reg==0 || retval$pd==0){
	    graded <- matrix(1,nrow=1,ncol=1)
	 }
	 else{
		 graded <- matrix(retval$graded[1:(retval$reg*(retval$pd+1))],
							byrow=TRUE,
	                  nrow=retval$reg,ncol=retval$pd+1)
	 }
	 bettis <- apply(graded,2,sum)
	if(verbose){
	   cat(retval$NC,"nodes in recursion tree\n")
	}
	graded <- graded[,bettis>0]
	bettis <- bettis[bettis>0]
	 list(bettis=bettis,graded=graded,reg=nrow(graded),pd=ncol(graded)-1,
	      punted=retval$punted)
}

mfr <- function(g,verbose=FALSE,nocode=FALSE,seed=42126,quiet=FALSE,
					 check.database=TRUE,
                suppress.warning=FALSE,add.internal=TRUE,remove.isolates=TRUE)
{
	if(!is.simple(g)) g <- simplify(g)
	if(is.directed(g)) g <- directed2bipartite(g,add.internal=add.internal,remove.isolates=remove.isolates)
	if(ecount(g)==0) {
	   m <- list(bettis=1,pd=0,reg=1,graded=matrix(1,nrow=1),punted=0)
		class(m) <- c("mfr","exact")
	   return(m)
	}
	if(check.database){
	   a <- checkDB(g)
		if(!is.null(a)) return(a)
	}
	if(seed>0){
		set.seed(seed)
		atrandom=FALSE
	} else {
	   atrandom=TRUE
	}
	if(nocode==FALSE){
		if(!has.singular()) {
			nocode <- TRUE
			if(!suppress.warning){
				warning("Singular not found: setting nocode==TRUE")
			}
		}
	}
	punts <- 0
	if(nocode){
	   if(!is.chordal(g)){
		   cat("The graph is not chordal, and nocode==TRUE\n")
			cat("The results are likely to be approximate\n")
		}
	}
	g <- removeIsolates(g)
   comps <- clusters(g)
	mfrx <- mfr1(subgraph(g,which(comps$membership==0)-1),
					 check.database=check.database,
	             verbose=verbose,nocode=nocode,atrandom=atrandom,quiet=quiet)
	punts <- mfrx$punted
	mfra <- mfrx$graded
	if(comps$no>1){
	   for(i in 1:(comps$no-1)){
			 mfry <- mfr1(subgraph(g,which(comps$membership==i)-1),
							  check.database=check.database,
							  verbose=verbose,nocode=nocode,atrandom=atrandom,
							  quiet=quiet)
		    mfra <- combineGraded(mfra,mfry$graded)
			 punts <- punts + mfry$punted
		}
	}
	bettis <- apply(mfra,2,sum)
	graded <- mfra[,bettis>0]
	bettis <- bettis[bettis>0]
	if(nocode && punts>0){
		if(!suppress.warning){
			warning("Non-chordal graph without Singular: results are approximate")
		}
	}
	m <- list(bettis=bettis,graded=graded,reg=nrow(mfra),pd=ncol(mfra)-1,
	     punted=punts)
   class(m) <- c("mfr",ifelse((punts>0) && nocode,"approximate","exact"))
	m
}

print.mfr <- function(x,...)
{
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
	if(inherits(x,"exact")) cat("MFR is exact\n")
	if(inherits(x,"approximate")) cat("MFR is approximate\n")
}

