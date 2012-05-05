
graph2BinomialEdgeIdeal <- function(g,resString="res")
{
	if(!(resString %in% c("res","lres","mres","sres","nres","minres")))
	   stop("Invalid resString")
	n <- vcount(g)
	s <- ecount(g)
	if(s==0) return(NULL)
	a <- paste("ring R=0, (x(1..",n,"),y(1..",n,")), dp; ",sep="");
	a <- paste(a,"ideal I =");
	edges <- get.edgelist(g)
	if(s>1){
		for(i in 1:(s-1)){
			a <- paste(a,"x(",edges[i,1]+1,")*y(",edges[i,2]+1,")-y(",edges[i,1]+1,")*x(",edges[i,2]+1,"), ",
			           sep="")
		}
	}
	a <- paste(a,"x(",edges[s,1]+1,")*y(",edges[s,2]+1,")-y(",edges[s,1]+1,")*x(",edges[s,2]+1,"); ",sep="")
	a <- paste(a,"resolution fI=",resString,"(I,0); ",sep="");
	a <- paste(a,"print(betti(fI)); ");
	a <- paste(a,"quit;\n");
	a
}

mfrBinomialEdge <- function(g,resString="res")
{
	if(is.complete(g)) return(MFRComplete(vcount(g)))
	if(!has.singular()){
	   stop("Cannot compute the MFR of the binomial edge ideal without Singular")
	}
	a <- singular(command=graph2BinomialEdgeIdeal(g,resString=resString))
   b <- lapply(strsplit(a,split="[[:blank:]]"),function(x) as.numeric(x[nchar(x)>0]))
	nc <- length(b[[1]])
	nr <- length(b)
	graded <- matrix(unlist(b),nrow=nr,byrow=TRUE)
	bettis <- colSums(graded)
	m <- list(graded=graded,punted=1,bettis=bettis,reg=nr,pd=nc-1)
	class(m) <- c("mfr","exact")
	m
}
