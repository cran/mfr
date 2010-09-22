is.connected <- function(g)
{
   all(components(g)==1)
}

components <- function(g)
{
   n <- Order(g)
	comp <- rep(0,n)
	curcomp <- 1
	v <- V(g)
	if(length(v)<n){
	   comp[-v] <- 1:(n-length(v))
		curcomp <- n-length(v)+1
	}
	while(any(comp==0)){
	   a <- which(comp==0)[1]
		b <- neighborhood(g,a)
		while(length(b)>length(a)){
		   a <- b
			b <- neighborhood(g,a)
		}
		comp[a] <- curcomp
		curcomp <- curcomp+1
	}
	comp
}

