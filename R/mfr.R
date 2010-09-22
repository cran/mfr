removeIsolates <- function(g)
{
	if(is.null(g)) return(NULL)
   v <- V(g)
	matrix(match(g,v),ncol=2)
}

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

mfr1 <- function(g,verbose=TRUE,nocode=FALSE,seed=33632)
{
   n <- Order(g)
	s <- Size(g)

	graded <- rep(0,n*n)

	tempname <- tempfile(paste("singular",n,s,"MFR",sep="_"))
	if(verbose){
	   cat("Graph = (",n,",",s,")\n",sep="")
		cat("tempname =",tempname,"\n")
	}
	punted <- 0
	retval <- .C("mfr",
					 edges1 = as.integer(g[,1]),
					 edges2 = as.integer(g[,2]),
					 N = as.integer(n),
					 S = as.integer(s),
					 NC = integer(1),
					 V = as.integer(verbose),
					 graded = as.double(graded),
					 pd = integer(1),
					 reg = integer(1),
					 punted = as.integer(punted),
					 dontPunt=as.integer(nocode),
					 seed=as.integer(seed),
					 tempname=as.character(tempname),
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

mfr <- function(g,verbose=FALSE,nocode=FALSE,seed=42126)
{
	if(nocode==FALSE){
		if(!has.singular()) {
			nocode <- TRUE
			warning("Singular not found: setting nocode==TRUE")
		}
	}
	punts <- 0
	if(Size(g)==0) 
	   return(list(bettis=1,pd=0,reg=1,graded=matrix(1,nrow=1),punted=0))
	if(nocode){
	   if(!is.chordal(g)){
		   cat("The graph is not chordal, and nocode==TRUE\n")
			cat("The results are likely to be approximate\n")
		}
	}
	g <- removeIsolates(g)
   comps <- components(g)
	mfrx <- mfr1(subgraph(g,which(comps==1)),verbose=verbose,nocode=nocode,
	             seed=seed)
	punts <- mfrx$punted
	mfra <- mfrx$graded
	if(max(comps)>1){
	   for(i in 2:max(comps)){
			 mfry <- mfr1(subgraph(g,which(comps==i)),
							  verbose=verbose,nocode=nocode,seed=seed)
		    mfra <- combineGraded(mfra,mfry$graded)
			 punts <- punts + mfry$punted
		}
	}
	bettis <- apply(mfra,2,sum)
	graded <- mfra[,bettis>0]
	bettis <- bettis[bettis>0]
	if(nocode && punts>0)
		warning("Non-chordal graph without Singular: results are approximate")
	list(bettis=bettis,graded=graded,reg=nrow(mfra),pd=ncol(mfra)-1,
	     punted=punts)
}

