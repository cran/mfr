
rg <- function(n,p=0.5)
{
	 if(any(p<0) || any(p>1)) 
	    stop("p must be a probability or matrix of probabilities")
	 edges <- NULL
	 if(is.matrix(p)){
		 for(i in 1:(n-1)){
			 for(j in (i+1):n){
				 if(rbinom(1,1,p[i,j])){
					 edges <- rbind(edges,c(i,j))
				 }
			 }
		 }
	 }
	 else {
		 for(i in 1:(n-1)){
			 for(j in (i+1):n){
				 if(rbinom(1,1,p)){
					 edges <- rbind(edges,c(i,j))
				 }
			 }
		 }
	 }
	 edges
}
