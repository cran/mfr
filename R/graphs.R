
cycleGraph <- function(n)
{
	if(n<=2) stop("n must be at least 3")
   cbind(1:n,c(2:n,1))
}

starGraph <- function(n)
{
	if(n<=1) stop("n must be at least 2")
   cbind(rep(n,n-1),1:(n-1))
}

Star <- function(g)
{
   vertices <- V(g)
	n <- length(vertices)
   rbind(g,cbind(vertices,rep(n+1,n)))
}

wheelGraph <- function(n)
{
   Star(cycleGraph(n-1))
}

brokenWheelGraph <- function(spokes)
{
   g <- cycleGraph(2*spokes)
	v <- 2*spokes+1
	rbind(g,cbind((1:spokes)*2,rep(v,spokes)))
}

ladderGraph <- function(n)
{
   g <- pathGraph(n)
   h <- pathGraph(n)+n
	E <- rbind(g,h)
	rbind(E,cbind(1:n,(1:n)+n))
}

completeGraph <- function(n)
{
	if(n<=1) stop("n must be at least 2")
   edges <- matrix(0,nrow=choose(n,2),ncol=2)
	k <- 1
	j <- 2
	for(i in 1:choose(n,2)){
	   edges[i,] <- c(k,j)
		j <- j+1
		if(j>n){
			k <- k+1
		   j <- k+1
		}
	}
	edges
}

completeBipartiteGraph <- function(n,m)
{
	if(n<1 || m<1) stop("n and m must be at least 1")
   edges <- matrix(0,nrow=n*m,ncol=2)
	k <- 1
	for(i in 1:n){
	   for(j in 1:m){
		   edges[k,] <- c(i,j+n)
			k <- k+1
		}
	}
	edges
}

pathGraph <- function(n)
{
	if(n<=1) stop("n must be at least 2")
   cbind(1:(n-1),2:n)
}

petersen <- function()
{
   rbind(
	   c(1,2),
		c(1,5),
		c(1,6),
		c(2,3),
		c(2,7),
		c(3,4),
		c(3,8),
		c(4,5),
		c(4,9),
		c(5,10),
		c(6,9),
		c(6,8),
		c(7,9),
		c(7,10),
		c(8,10))
}

petersenX <- function()
{
  rbind(
  c(0.0000000,2.000000),
  c(1.9021130,0.618034),
  c(1.1755705,-1.618034),
  c(-1.1755705,-1.618034),
  c(-1.9021130,0.618034),
  c(0.0000000,1.000000),
  c(0.9510565,0.309017),
  c(0.5877853,-0.809017),
  c(-0.5877853,-0.809017),
  c(-0.9510565,0.309017))
}
