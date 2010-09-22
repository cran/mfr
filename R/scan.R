scanMFR <- function(g,open=FALSE,...)
{
   n <- Order(g)
	out <- list(0)
	v <- V(g)
   for(i in 1:n){
		if(i %in% v){
			N <- neighborhood(g,i,open=open)
			out[[i]] <- mfr(subgraph(g,N),...)
		}
		else{
		   out[[i]] <- list(bettis=1,graded=matrix(1,nrow=1),pd=0,rg=1,punted=0)
		}
	}
	out
}

meanScanMFR <- function(m)
{
   rg <- max(unlist(lapply(m,function(x)x$reg)))
   pd <- max(unlist(lapply(m,function(x)x$pd)))
   p <- max(unlist(lapply(m,function(x)x$punted)))
	G <- matrix(0,nrow=rg,ncol=pd+1)
	b <- rep(0,pd+1)
	d <- rep(0,pd+1)
	D <- matrix(0,nrow=rg,ncol=pd+1)
	for(i in 1:length(m)){
	   a <- m[[i]]$bettis
		if(length(a)>0){
			b[1:length(a)] <- b[1:length(a)]+a
			d[1:length(a)] <- d[1:length(a)]+1
			a <- m[[i]]$graded
			G[1:nrow(a),1:ncol(a)] <- G[1:nrow(a),1:ncol(a)]+a
			for(j in 1:nrow(a)){
				for(k in 1:ncol(a)){
					if(G[j,k]>0)
					D[j,k] <- D[j,k]+1
				}
			}
		}
	}
	for(i in 1:ncol(D)){
		if(d[i]>0) b[i] <- b[i]/d[i]
		for(j in 1:nrow(D)){
			if(D[j,i]>0) G[j,i] <- G[j,i]/D[j,i]
		}
	}
	list(bettis=b,graded=G,reg=rg,pd=pd,punted=p,D=D)
}

uniqueScanMFR <- function(m)
{
	G <- lapply(m,function(x)x$graded)
	U <- unique(G)
	counts <- rep(0,length(U))
	for(i in 1:length(U)){
		u <- U[[i]]
		for(j in 1:length(G)){
			g <- G[[j]]
		   if((nrow(u)==nrow(g)) && (ncol(u)==ncol(g))){
			   if(all(u==g)){
		         counts[i] <- counts[i]+1
				}
			}
		}
	}
	list(graded=U,count=counts)
}
