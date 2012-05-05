clique.expand <- function(g=graph.full(4,directed=FALSE))
{
	if(is.directed(g)) {
	   warning("g is directed...converting to undirected")
		g <- as.undirected(g)
	}
	if(!is.simple(g)){
	   warning("g is not simple...simplifying")
		g <- simplify(g)
	}
	n <- vcount(g)
   d <- degree(g)
	if(any(d!=d[1])) stop("g must be regular")
	d <- d[1]
	if(d<2) stop("g must be regular of degree at least 2")
	X <- get.graph.attribute(g,name="layout")
	if((d==(n-1)) && (is.null(X))){
	   X <- layout.concentric.circles(g)
	}
	if(!is.null(X)){
	   r <- min(dist(X))/d
		if(r==0) r <- 1
		Y <- NULL
	}

	g1 <- g
	for(i in 1:n){
		EDGES <- get.edgelist(g1)
		v <- i-1
		m <- vcount(g1)
		N <- neighborhood(g1,order=1,nodes=v)[[1]]
		verts <- setdiff(N,0:v)
		g1 <- graph.disjoint.union(g1,graph.full(d,directed=FALSE))
		edges <- rbind(m:(m+d-1),verts)
		g1 <- add.edges(g1,edges)
		edges <- rep(NA,length(verts))
		for(j in 1:length(verts)){
			x <- c(v,verts[j])
		   edges[j] <- which((EDGES[,1] %in% x) & (EDGES[,2] %in% x))-1
		}
		if(!is.null(X)){
		   a <- layout.circle(graph.full(d,directed=FALSE))
			z <- scale(a,center=-X[i,],scale=rep(1/r,2))
			Y <- rbind(Y,z)
		}
		delete.edges(g1,edges)
	}
	g1 <- delete.vertices(g1,0:(n-1))
	if(!is.null(Y)) g1 <- set.graph.attribute(g1,name="layout",value=Y)
	g1
}
