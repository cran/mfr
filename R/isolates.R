
removeIsolates <- function(g)
{
	if(is.null(g)) return(NULL)
   v <- V(g)
	d <- degree(g)
	delete.vertices(g,which(d==0)-1)
}

isolates <- function(g)
{
   I <- which(degree(g)==0)
	if(length(I)==0) return(NULL)
	I-1
}

