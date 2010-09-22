subgraph <- function(g,U,remove=TRUE)
{
	E <- NULL
	for (i in 1:Size(g)) {
		if (all(g[i,] %in% U)) {
			E <- rbind(E, g[i, ])
		}
	}
	if(remove)
		return(removeIsolates(E))
   E
}
