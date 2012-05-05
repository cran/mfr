graph.barbell <- function(n1=4,n2=4,p=1,full=TRUE)
{
	if(full){
		g1 <- graph.full(n1,directed=FALSE)
	   g2 <- graph.full(n2,directed=FALSE)
	} else {
		g1 <- graph.ring(n1,directed=FALSE)
	   g2 <- graph.ring(n2,directed=FALSE)
	}
	g <- graph.disjoint.union(g1,g2)
	if(p>0){
		if(p==1){
		   g <- add.edges(g,c(0,n1+n2-1))
		} else {
			edges <- cbind(0:(p-2),1:(p-1))
			g <- graph.disjoint.union(g,graph.edgelist(edges,directed=FALSE))
			g <- add.edges(g,cbind(c(0,n1+n2),c(n1,vcount(g)-1)))
		}
	}
	g
}
