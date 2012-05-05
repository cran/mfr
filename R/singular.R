
#has.singular <- function(){
#	if(Sys.info()[['sysname']]=="Windows"){
#		a <- system("bash.exe Singular -c 'quit;' -q -t --no-out --no-warn",
#		       show.output.on.console=FALSE,minimized=TRUE,invisible=TRUE)
#	} else {
#		a <- system("Singular -c 'quit;' -q -t --no-out --no-warn",
#		            ignore.stdout=TRUE,ignore.stderr=TRUE)
#	}
#	a==0
#}
has.singular <- function(){
	if(Sys.info()[['sysname']]=="Windows"){
		# this is never going to work on a windows box
		a <- system("Singular -c 'quit;' -q -t --no-out --no-warn",
		       show.output.on.console=FALSE,minimized=TRUE,invisible=TRUE)
	} else {
		a <- system("Singular -c 'quit;' -q -t --no-out --no-warn",
		            ignore.stdout=TRUE,ignore.stderr=TRUE)
	}
	a==0
}

singular.version <- function(verbose=FALSE)
{
   if(has.singular()==FALSE) return(NULL)
	if(Sys.info()[['sysname']]=="Windows"){
		a <- system("bash.exe Singular -c 'quit;' -q -v",intern=TRUE)
	} else {
		a <- system("Singular -c 'quit;' -q -v",
						intern=TRUE,ignore.stderr=TRUE)
	}
	if(!verbose) a <- a[1]
	return(a)

}

graph2EdgeIdeal <- function(g,resString="res")
{
	if(!(resString %in% c("res","lres","mres","sres","nres","minres")))
	   stop("Invalid resString")
	n <- vcount(g)
	s <- ecount(g)
	if(s==0) return(NULL)
	a <- paste("ring R=0, (x(1..",n,")), dp; ",sep="");
	a <- paste(a,"ideal I =");
	edges <- get.edgelist(g)
	if(s>1){
		for(i in 1:(s-1)){
			a <- paste(a,"x(",edges[i,1]+1,")*x(",edges[i,2]+1,"), ",sep="")
		}
	}
	a <- paste(a,"x(",edges[s,1]+1,")*x(",edges[s,2]+1,"); ",sep="")
	a <- paste(a,"resolution fI=",resString,"(I,0); ",sep="");
	a <- paste(a,"print(betti(fI),\"betti\"); ");
	a <- paste(a,"quit;\n");
	a
}

singular <- function(g,verbose=FALSE,command,quiet=FALSE)
{
	if(missing(g)){
		if(missing(command)) stop("must provide a graph or a command")
		if(!is.character(command)) stop("command must be a character string")
		if(Sys.info()[['sysname']]=="Windows"){
			a <- system(paste("bash.exe Singular -c",
			                  paste("'",command," \nquit;'",sep=""),
									"-q -t"),
							intern=TRUE)
		} else {
			a <- system(paste("Singular -c",paste("'",command," \nquit;'",sep=""),
									"-q -t"),
							intern=TRUE)
		}
		return(a)
	}
   n <- vcount(g)
	s <- ecount(g)
	edges <- get.edgelist(g)+1

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
					 edges1 = as.integer(edges[,1]),
					 edges2 = as.integer(edges[,2]),
					 N = as.integer(n),
					 S = as.integer(s),
					 V = as.integer(verbose),
					 graded = as.double(graded),
					 pd = integer(1),
					 reg = integer(1),
					 punted = as.integer(punted),
					 tempname=as.character(tempname),
					 quiete=as.integer(quiet),
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
	 a <- list(bettis=bettis,graded=graded,reg=retval$reg,pd=retval$pd,
	      punted=retval$punted)
    class(a) <- c("mfr","exact")
	 a
}

