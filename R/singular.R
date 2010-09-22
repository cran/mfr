has.singular <- function(){
	system("which Singular > /dev/null",ignore.stderr=TRUE)==0
}

singular <- function(g,verbose=FALSE)
{
   n <- Order(g)
	s <- Size(g)

	if(s==0) 
	   return(list(bettis=1,pd=0,reg=1,graded=matrix(1,nrow=1),punted=0))

	graded <- rep(0,n*n)

	tempname <- tempfile(paste("singular",n,s,"CODE",sep="_"))
	if(verbose){
	   cat("Graph = (",n,",",s,")\n",sep="")
		cat("tempname =",tempname,"\n")
	}
	punted <- 0
	retval <- .C("Singular",
					 edges1 = as.integer(g[,1]),
					 edges2 = as.integer(g[,2]),
					 N = as.integer(n),
					 S = as.integer(s),
					 V = as.integer(verbose),
					 graded = as.double(graded),
					 pd = integer(1),
					 reg = integer(1),
					 punted = as.integer(punted),
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
	 list(bettis=bettis,graded=graded,reg=retval$reg,pd=retval$pd,
	      punted=retval$punted)
}

