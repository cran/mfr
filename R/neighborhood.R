neighborhood <- function(g,v,open=FALSE)
{
	V <- v
	z <- which(g[,1] %in% v)
	if(length(z)>0) V <- union(V,g[z,2])
	z <- which(g[,2] %in% v)
	if(length(z)>0) V <- union(V,g[z,1])
	if(open) V <- setdiff(V,v) # done this way in case v is a vector
	V
}
