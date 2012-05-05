graph.truncated.tetrahedron <- function(vertex.colors=c(2,4),
                                  edge.colors=c(1,3))
{
	ec <- matrix(0,nrow=18,ncol=2)

	ec[1,] <- c(0,1)
	ec[2,] <- c(0,5)
	ec[3,] <- c(0,8)
	ec[4,] <- c(1,2)
	ec[5,] <- c(1,8)
	ec[6,] <- c(2,3)
	ec[7,] <- c(2,7)
	ec[8,] <- c(3,4)
	ec[9,] <- c(3,7)
	ec[10,] <- c(4,5)
	ec[11,] <- c(4,6)
	ec[12,] <- c(5,6)
	ec[13,] <- c(6,9)
	ec[14,] <- c(7,10)
	ec[15,] <- c(8,11)
	ec[16,] <- c(9,10)
	ec[17,] <- c(9,11)
	ec[18,] <- c(10,11)

	g <- graph(t(ec),directed=FALSE)

	n <- vcount(g)

	theta <- c(0,2*pi/3,4*pi/3)
	x1 <- cbind(cos(theta),sin(theta))
	x2 <- 2*x1
	theta1 <- theta+pi/6
	theta2 <- theta-pi/6
	x3 <- 3.75*cbind(cos(theta1),sin(theta1))
	x4 <- 3.75*cbind(cos(theta2),sin(theta2))

	X <- matrix(0,nrow=n,ncol=2)
	X[12,] <- x1[1,]
	X[11,] <- x1[2,]
	X[10,] <- x1[3,]
	X[9,] <- x2[1,]
	X[8,] <- x2[2,]
	X[7,] <- x2[3,]
	X[2,] <- x3[1,]
	X[4,] <- x3[2,]
	X[6,] <- x3[3,]
	X[1,] <- x4[1,]
	X[3,] <- x4[2,]
	X[5,] <- x4[3,]

	g <- set.graph.attribute(g,name="layout",value=X)


	R <- vertex.colors[1]
	B <- vertex.colors[2]
	G <- edge.colors[1]
	Y <- edge.colors[2]

	vc <- c(R,B,R,R,B,R,B,B,B,R,B,R)
	ec <- c(G,G,Y,G,Y,G,Y,G,Y,G,Y,Y,G,G,G,Y,Y,G)
	g <- set.vertex.attribute(g,name="color",value=vc)
	g <- set.edge.attribute(g,name="color",value=ec)
	g

}

graph.martini.glasses <- function(vertex.colors=c(2,4),
									 edge.colors=c(1,3))
{
   g1 <- graph.barbell(3,3,1)
	g1 <- add.edges(g1,c(2,3))
	g2 <- graph.barbell(3,3,1)
	g2 <- add.edges(g2,c(2,3))
	g <- graph.disjoint.union(g1,g2)
	g <- add.edges(g,cbind(c(1,7),c(4,10)))

	n <- 12
	X <- matrix(0,nrow=n,ncol=2)
	X[1,] <- c(1,2)
	X[2,] <- c(2,3)
	X[3,] <- c(0,3)
	X[4,] <- c(0,0)
	X[5,] <- c(2,0)
	X[6,] <- c(1,1)
	X[7,] <- c(5,2)
	X[8,] <- c(4,3)
	X[9,] <- c(6,3)
	X[10,] <- c(6,0)
	X[11,] <- c(4,0)
	X[12,] <- c(5,1)
	g <- set.graph.attribute(g,name="layout",value=X)

	R <- vertex.colors[1]
	B <- vertex.colors[2]
	G <- edge.colors[1]
	Y <- edge.colors[2]

	vc <- c(R,B,R,B,R,B,
			  R,B,R,B,R,B)
	ec <- c(G,Y,Y,G,Y,Y,G,G,G,
			  Y,Y,G,Y,Y,G,G,G,Y)
	g <- set.vertex.attribute(g,name="color",value=vc)
	g <- set.edge.attribute(g,name="color",value=ec)
	g
	
}
