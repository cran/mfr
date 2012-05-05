
kidney.egg.game <- function(n=100,p=.1,m=10,q=.4,r=p,g,h,
                      ktype="gnp",etype="gnp",ektype="gnp",mode="out",
                      directed=FALSE,loops=FALSE,add.to=FALSE)
{
	if(!missing(g)) {
		n <- vcount(g)
	}
	if(!missing(h)) {
		m <- vcount(h)
	}
	if(m>(n/2)){
		stop("m must be less than or equal to n/2")
	} else if(m<1){
	   stop("m must be positive")
	}
	if(ektype=="gnm"){
	   if((r<0) || (r>(n*m))) 
		   stop(paste("invalid r given: must be between 0 and",n*m))
	} else {
	   if((r<0) || r>1) {
			stop("invalid r given: must be between 0 and 1")
		}
	}
	nog <- missing(g)
	noh <- missing(h)

	if(noh){
		if(m==1){
		   h <- graph.empty(1,directed=directed)
		} else {
			h <- erdos.renyi.game(m,p.or.m=q,directed=directed,type=etype,
										 loops=loops)
		}
	} 
	if(nog){
        if (add.to) {
            g <- erdos.renyi.game(n, p.or.m = p, directed = directed, 
                type = ktype, loops = loops)
        }
        else {
				g <- erdos.renyi.game(n-m,p.or.m=p,directed=directed,type=ktype,
										 loops=loops)
		} 
	} else {
        if (!add.to) {
            g <- delete.vertices(g, 0:(m - 1))
        }
	}

	n <- vcount(g)
	m <- vcount(h)
	if(ektype=="gnm") {
	   g <- product.game(h,g,n=p)
	} else {
	   g <- product.game(h,g,p=p)
	}

	n <- vcount(g)
	x <- matrix(0,nrow=n,ncol=2)

	gx <- get.graph.attribute(g,name="layout")
	if(is.null(gx)){
		theta <- seq(pi/4+5/n,7*pi/4-5/n,length=n-m)
		gx <- 2*cbind(cos(theta),sin(theta))
	} 
	x[-(1:m),] <- gx
	z1 <- max(gx[,1])
	hx <- get.graph.attribute(h,name="layout")
	if(is.null(hx)){
		theta <- seq(0,2*pi,length=m+1)[-1]
		x[1:m,] <- cbind(cos(theta),sin(theta))
	} else {
	   x[1:m,] <- hx
	}
	z2 <- min(x[1:m,1])
	x[1:m,1] <- x[1:m,1] + (z1-z2)+1
	g <- set.graph.attribute(g,name="layout",value=x)
	g
}

kidneyEgg <- kidney.egg.game
