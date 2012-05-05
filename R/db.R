estimate.mfr.time <- function(g)
{
	s <- ecount(g)
	(exp(s/3-7.5367)+
	exp(0.3669*s-6.8894))/2
}

valid4DB <- function(g,maxTime,checkSplitting=TRUE)
{
	if(ecount(g)==0) return(FALSE)
   if(no.clusters(g)>1) return(FALSE)
	if(estimate.mfr.time(g)>maxTime) return(FALSE)
	if(checkSplitting){
		return(splittingEdge(g)==0)
	}
	TRUE
}

which.DB <- function(g,db)
{
	n <- vcount(g)
	if(missing(db)){
	   db <- mfrDB
	}
	a <- which(db$orders==n)
	if(length(a)>0){
	   if(length(a)==1){
		   if(graph.isomorphic(g,db$graphs[[a]])){
			   return(a)
			} else {
			   return(NULL)
			}
		} else {
			graphs <- db$graphs[a]
			mfrs <- db$mfrs[a]
			b <- which(unlist(lapply(graphs,function(x) graph.isomorphic(x,g))))
			if(length(b)>0){
				return(a[b[1]])
			} else {
			  return(NULL)
			}
		}
	}
	NULL
}

checkDB <- function(g)
{
	n <- vcount(g)
	a <- which.DB(g)
	if(!is.null(a)){
	   m <- mfrDB$mfrs[[a]]
		m$punted <- 0
	} else {
	   m <- NULL
	}
	m
}

atlas2DB <- function()
{
		mfrDB <- list(graphs=vector("list",20),mfrs=vector("list",20),
						  sizes=rep(0,20),
						  orders=rep(0,20),timings=vector("list",20),
						  n=0)
		k <- 1
		for(i in 1:1252){
			g <- graph.atlas(i)
			if(valid4DB(g,100000,checkSplitting=TRUE)){
				mfrDB$timings[[k]] <- system.time(mf <- mfr(g,check.database=FALSE))
				if(mf$punted>0){
					mfrDB$orders[k] <- vcount(g)
					mfrDB$sizes[k] <- ecount(g)
					mfrDB$graphs[[k]] <- g
					mfrDB$mfrs[[k]] <- mf
					k <- k+1
				}
			}
		}
		mfrDB$n <- k-1
		mfrDB
}

famous2DB <- function(mfrDB)
{
	# two slow ones are precomputed below.
	famous.graphs <- c("Chvatal","Cubical",#"Dodecahedral","Folkman",
	   "Franklin","Frucht","Grotzsch","Heawood","Herschel",
		"Icosahedral","Krackhardt_Kite","Noperfectmatching",
		"Petersen","Smallestcyclicgroup","Uniquely3colorable")
	k <- mfrDB$n+1
	for(i in 1:length(famous.graphs)){
		g <- graph.famous(famous.graphs[i])
		if(valid4DB(g,100000,checkSplitting=FALSE)){
			cat(famous.graphs[i],"\n")
			mfrDB$timings[[k]] <- system.time(mf <- mfr(g,check.database=FALSE,
															  suppress.warning=TRUE))
			if(mf$punted>0){
				mfrDB$graphs[[k]] <- g
				mfrDB$orders[k] <- vcount(g)
				mfrDB$sizes[k] <- ecount(g)
				mfrDB$mfrs[[k]] <- mf
				k <- k+1
			}
		}
	}
	# "Dodecahedral","Folkman" precomputed
	cat("Dodecahedral\n")
	g <- graph.famous("Dodecahedral")
	mfrDB$graphs[[k]] <- g
	mfrDB$orders[[k]] <- vcount(g)
	mfrDB$sizes[k] <- ecount(g)
	mfrDB$timings[[k]] <- mfrDB$timings[[k-1]]
	mfrDB$timings[[k]]['user'] <- 163.450
	mfrDB$timings[[k]]['system'] <- 6.804
	mfrDB$timings[[k]]['elapsed'] <- 170.503
	mfrDB$mfrs[[k]] <- mfrDB$mfrs[[k-1]]
	mfrDB$mfrs[[k]]$reg <- 7
	mfrDB$mfrs[[k]]$pd <- 14
	mfrDB$mfrs[[k]]$punted <- 1
	mfrDB$mfrs[[k]]$graded <- rbind(
	c(1,   0,   0,   0,   0,   0,    0,    0,    0,    0,    0,    0,    0,0,0),
	c(0,  30,  60,  20,   0,   0,    0,    0,    0,    0,    0,    0,    0,0,0),
	c(0,   0, 255,1032,1380, 660,  100,    0,    0,    0,    0,    0,    0,0,0),
	c(0,   0,   0, 620,3820,9120,10260, 5456, 1260,  100,    0,    0,    0,0,0),
	c(0,   0,   0,   0, 315,2520, 8676,16120,16280, 8820, 2290,  240,   10,0,0),
	c(0,   0,   0,   0,   0,  30,  240,  840, 1740, 2490, 2232, 1000,  180,0,0),
	c(0,   0,   0,   0,   0,   0,    5,   40,  140,  280,  350,  280,  140,40,4))
   k <- k+1
	cat("Folkman\n")
	g <- graph.famous("Folkman")
	mfrDB$graphs[[k]] <- g
	mfrDB$orders[[k]] <- vcount(g)
	mfrDB$sizes[k] <- ecount(g)
	mfrDB$timings[[k]] <- mfrDB$timings[[k-1]]
	mfrDB$timings[[k]]['user'] <- 2447.813
	mfrDB$timings[[k]]['system'] <- 103.826
	mfrDB$timings[[k]]['elapsed'] <- 2554.873
	mfrDB$mfrs[[k]] <- mfrDB$mfrs[[k-1]]
	mfrDB$mfrs[[k]]$reg <- 5
	mfrDB$mfrs[[k]]$pd <- 16
	mfrDB$mfrs[[k]]$punted <- 1
	mfrDB$mfrs[[k]]$graded <- rbind(
	c(1,   0,   0,   0,   0,    0,    0,    0,    0,    0,    0,    0,0,0,0,0,0),
	c(0,  40, 120, 110,  40,    5,    0,    0,    0,    0,    0,    0,0,0,0,0,0),
	c(0,   0, 360,2160,5170, 6450, 4600, 1900,  425,   40,    0,    0,0,0,0,0,0),
	c(0,   0,   0, 640,5600,21680,47600,66224,61350,38395,16040, 4285,662,45,0,0,0),
	c(0,   0,   0,   0,  80,  800, 3640,10280,20005,27240,25980,17388,8140,2620,555,70,4))
	cat("Famous done\n",k,"\n")
	print(table(mfrDB$orders))
   k <- k+1

	g <- graph.truncated.tetrahedron()
	a <- which.DB(g,mfrDB)
	if(is.null(a)){
		mfrDB$graphs[[k]] <- g
		mfrDB$orders[k] <- vcount(g)
		mfrDB$sizes[k] <- ecount(g)
		mfrDB$timings[[k]] <- system.time(mf <- mfr(g))
		mfrDB$mfrs[[k]] <- mf
	}
	g <- graph.martini.glasses()
	a <- which.DB(g,mfrDB)
	if(is.null(a)){
		k <- k+1
		mfrDB$graphs[[k]] <- g
		mfrDB$orders[k] <- vcount(g)
		mfrDB$sizes[k] <- ecount(g)
		mfrDB$timings[[k]] <- system.time(mf <- mfr(g))
		mfrDB$mfrs[[k]] <- mf
	}
	mfrDB$n <- k
	mfrDB
}

wheels2DB <- function(mfrDB,N=20)
{
	k <- mfrDB$n+1
	g <- graph.extended.chordal.ring(15, matrix(c(3,12,4,7,8,11), nrow=2))
	if(is.null(which.DB(g,mfrDB))){
		mfrDB$graphs[[k]] <- g
		mfrDB$orders[k] <- vcount(g)
		mfrDB$sizes[k] <- ecount(g)
		mfrDB$timings[[k]] <- system.time(mf <- mfr(g,check.database=FALSE,
														  suppress.warning=TRUE))
		mfrDB$mfrs[[k]] <- mf
		k <- k+1
	}
	for(n in 8:20){
	   if((n %% 2)==1){
			g <- graph.bwg(n)
			mfrDB$timings[[k]] <- system.time(mf <- MFRWheel(n))
		} else {
			g <- graph.wheel(n)
			mfrDB$timings[[k]] <- system.time(mf <- mfr(g,check.database=FALSE,
			                                      suppress.warning=TRUE))
		}
		if(is.null(which.DB(g,mfrDB))){
			mfrDB$graphs[[k]] <- g
			mfrDB$orders[k] <- vcount(g)
			mfrDB$sizes[k] <- ecount(g)
			mfrDB$mfrs[[k]] <- mf
			k <- k+1
		}
	}
	cat("Wheels done\n",k-1,"\n")
	print(table(mfrDB$orders))

	for(n1 in 4:7){
		for(n2 in 4:7){
		   for(p in 1:6){
			   g <- graph.barbell(n1,n2,p,full=FALSE)
				mfrDB$timings[[k]] <- system.time(mf <- mfr(g,check.database=FALSE,
			                                      suppress.warning=TRUE))
				if(is.null(which.DB(g,mfrDB))){
					mfrDB$graphs[[k]] <- g
					mfrDB$orders[k] <- vcount(g)
					mfrDB$sizes[k] <- ecount(g)
					mfrDB$mfrs[[k]] <- mf
					k <- k+1
				}
			}
		}
	}
	cat("Barbells done\n",k-1,"\n")
	print(table(mfrDB$orders))
	for(n1 in 4:10){
		g <- graph.ring(n1)
		for(n2 in 4:10){
			h <- graph.ring(n2)
			for(i in 1:N){
				h1 <- product.game(g,h,n=sample(2:10,1))
				a <- which.DB(h1,mfrDB)
				if(is.null(a)){
					mfrDB$timings[[k]] <- system.time(mf <- mfr(h1,
												suppress.warning=TRUE))
					if(mf$punted>0){
						mfrDB$mfrs[[k]] <- mf
						mfrDB$graphs[[k]] <- h1
						mfrDB$orders[k] <- vcount(h1)
						mfrDB$sizes[k] <- ecount(h1)
						k <- k+1
					}
				}
			}
		}
	}
	cat("Ring Products done\n",k-1,"\n")
	print(table(mfrDB$orders))

	mfrDB$n <- k-1
	mfrDB
}

kidney2DB <- function(mfrDB,N=1000,ns=15:20,p=.1,m=5,q=.8,verbose=TRUE,
							 maxTime=1200,
                      checkpoint=TRUE)
{
	if(verbose){
	   cat("Running",N,"kidney-egg graphs\nn =",
		    paste(min(ns),max(ns),sep="-"),"p =",p,
			 "m =",m,"q =",q,"\n")
	}
	t1 <- Sys.time()
	k <- mfrDB$n
	for(n in ns){
		if(verbose){
			t2 <- Sys.time()
			cat(n," ")
		}
		for(i in 1:N){
			h1 <- kidney.egg.game(n=n,p=p,m=m,q=q)
			j <- 1
			if(no.clusters(h1)>1){
				a <- clusters(h1)
				b <- which.max(a$csize)
				h1 <- subgraph(h1,which(a$membership==(b-1))-1)
			}
			if(valid4DB(h1,maxTime,checkSplitting=TRUE)){
				a <- which.DB(h1,mfrDB)
				if(is.null(a)){
					mfrDB$timings[[k+1]] <- system.time(mf <- mfr(h1,
					                     suppress.warning=TRUE))
					if(mf$punted>0){
						k <- k+1
						mfrDB$mfrs[[k]] <- mf
						mfrDB$graphs[[k]] <- h1
						mfrDB$orders[k] <- vcount(h1)
						mfrDB$sizes[k] <- ecount(h1)
					}
				}
				if(checkpoint){
					mfrDB$n <- k
					save(mfrDB,file="mfrDB.RData",compress="bzip2")
				}
			}
		}
		if(verbose){
			print(Sys.time()-t2)
		}
	}
	mfrDB$n <- k
	mfrDB
}

star2DB <- function(mfrDB,N=1000,ns=8:20,maxTime=1200,
                    verbose=TRUE,checkpoint=TRUE)
{
	if(verbose){
	   cat("Running",N,"star graphs\n")
	}
	t1 <- Sys.time()
	k <- mfrDB$n
	for(n in ns){
		t2 <- Sys.time()
		h <- graph.star(n,mode="undirected")
		pedges <- combn(n-1,2)
		np <- ncol(pedges)
		cat(n," ")
		for(i in 1:N){
			b <- 1:(n-1)
			edges <- NULL
			while(length(b)>0){
				if(length(b)==1){
					u <- sample(setdiff(1:(n-1),b),1)
					v <- b
				} else {
					u <- sample(1:(n-1),1)
					b <- setdiff(b,u)
					if(length(b)==1) {
						v <- b
					} else {
						v <- sample(b,1)
					}
				}
				edges <- unique(rbind(edges,c(min(u,v),max(u,v))))
				b <- setdiff(b,v)
			}
			h1 <- graph.union(h,graph(t(edges),directed=FALSE))
			if(valid4DB(h1,maxTime)){
				a <- which.DB(h1,mfrDB)
				if(is.null(a)){
					mfrDB$timings[[k+1]] <- system.time(mf <- mfr(h1,
					                                    suppress.warning=TRUE))
					if(mf$punted>0){
						k <- k+1
						mfrDB$graphs[[k]] <- h1
						mfrDB$mfrs[[k]] <- mf
						mfrDB$orders[k] <- vcount(h1)
						mfrDB$sizes[k] <- ecount(h1)
					}
				}
			}
			j <- sample(np,1)
			nedges <- pedges[,sample(np,j),drop=FALSE]
			g <- graph.union(h1,graph(nedges,directed=FALSE))
			if(valid4DB(g,maxTime)){
				a <- which.DB(g,mfrDB)
				if(is.null(a)){
					mfrDB$timings[[k+1]] <- system.time(mf <- mfr(g,
					                                    suppress.warning=TRUE))
					if(mf$punted>0){
						k <- k+1
						mfrDB$graphs[[k]] <- g
						mfrDB$mfrs[[k]] <- mf
						mfrDB$orders[k] <- vcount(g)
						mfrDB$sizes[k] <- ecount(g)
					}
				}
			}
		}
		if(checkpoint){
			mfrDB$n <- k
			save(mfrDB,file="mfrDB.RData",compress="bzip2")
		}
		if(verbose){
			print(Sys.time()-t2)
		}
	}
	mfrDB$n <- k
	mfrDB
}

er2DB <- function(mfrDB,N=1000,ns=8:20,maxTime=1200,p=.1,
                  verbose=TRUE,checkpoint=TRUE)
{
	if(verbose){
	   cat("Running",N,"ER graphs\n")
		cat("p =",p,"\n")
	}
	t1 <- Sys.time()
	k <- mfrDB$n
	for(n in ns){
		cat(n," ")
		t2 <- Sys.time()
		for(i in 1:N){
			g <- erdos.renyi.game(n,p.or.m=p)
			if(no.clusters(g)>1){
				a <- clusters(g)
				b <- which.max(a$csize)
				g <- subgraph(g,which(a$membership==(b-1))-1)
			}
			if(valid4DB(g,maxTime,checkSplitting=TRUE)){
				j <- which.DB(g,mfrDB)
				if(is.null(j)){
					mfrDB$timings[[k+1]] <- system.time(mf <- mfr(g,
					                             suppress.warning=TRUE))
					if(mf$punted>0){
						k <- k+1
						mfrDB$orders[k] <- vcount(g)
						mfrDB$sizes[k] <- ecount(g)
						mfrDB$graphs[[k]] <- g
						mfrDB$mfrs[[k]] <- mf
					}
				} 
			} 
		}
		if(checkpoint){
			mfrDB$n <- k
			save(mfrDB,file="mfrDB.RData",compress="bzip2")
		}
		if(verbose){
			print(Sys.time()-t2)
		}
	}
	mfrDB$n <- k
	mfrDB
}

grg2DB <- function(mfrDB,N=1000,ns=8:20,maxTime=1200,
                   r=.1,verbose=TRUE,checkpoint=TRUE)
{
	if(verbose){
	   cat("Running",N,"geometric graphs\n")
	}
	t1 <- Sys.time()
	k <- mfrDB$n
	for(n in ns){
		cat(n," ")
		t2 <- Sys.time()
		for(i in 1:N){
			g <- grg.game(n,radius=r)
			if(no.clusters(g)>1){
				a <- clusters(g)
				b <- which.max(a$csize)
				g <- subgraph(g,which(a$membership==(b-1))-1)
			}
			if(valid4DB(g,maxTime,checkSplitting=TRUE)){
				j <- which.DB(g,mfrDB)
				if(is.null(j)){
					mfrDB$timings[[k+1]] <- system.time(mf <- mfr(g,
					                          suppress.warning=TRUE))
					if(mf$punted>0){
						k <- k+1
						mfrDB$orders[k] <- vcount(g)
						mfrDB$sizes[k] <- ecount(g)
						mfrDB$graphs[[k]] <- g
						mfrDB$mfrs[[k]] <- mf
					}
				} 
			} 
			g <- grg.game(n,radius=r,torus=TRUE)
			if(no.clusters(g)>1){
				a <- clusters(g)
				b <- which.max(a$csize)
				g <- subgraph(g,which(a$membership==(b-1))-1)
			}
			if(valid4DB(g,maxTime,checkSplitting=TRUE)){
				j <- which.DB(g,mfrDB)
				if(is.null(j)){
					mfrDB$timings[[k+1]] <- system.time(mf <- mfr(g,
					                          suppress.warning=TRUE))
					if(mf$punted>0){
						k <- k+1
						mfrDB$orders[k] <- vcount(g)
						mfrDB$sizes[k] <- ecount(g)
						mfrDB$graphs[[k]] <- g
						mfrDB$mfrs[[k]] <- mf
					}
				} 
			} 
		}
		if(checkpoint){
			mfrDB$n <- k
			save(mfrDB,file="mfrDB.RData",compress="bzip2")
		}
		if(verbose){
			print(Sys.time()-t2)
		}
	}
	mfrDB$n <- k
	mfrDB
}

rdpg2DB <- function(mfrDB,N=1000,ns=8:20,ds=1:10,maxTime=1200,
                   verbose=TRUE,checkpoint=TRUE)
{
	if(verbose){
	   cat("Running",N,"rdpg graphs\n")
	}
	t1 <- Sys.time()
	k <- mfrDB$n
	for(n in ns){
		cat(n," ")
		t2 <- Sys.time()
		for(d in ds){
			for(i in 1:N){
				g <- rdpg.game(n=n,d=d,directed=FALSE)
				if(no.clusters(g)>1){
					a <- clusters(g)
					b <- which.max(a$csize)
					g <- subgraph(g,which(a$membership==(b-1))-1)
				}
				if(valid4DB(g,maxTime,checkSplitting=TRUE)){
					j <- which.DB(g,mfrDB)
					if(is.null(j)){
						mfrDB$timings[[k+1]] <- system.time(mf <- mfr(g,
														  suppress.warning=TRUE))
						if(mf$punted>0){
							k <- k+1
							mfrDB$orders[k] <- vcount(g)
							mfrDB$sizes[k] <- ecount(g)
							mfrDB$graphs[[k]] <- g
							mfrDB$mfrs[[k]] <- mf
						}
					} 
				} 
			}
			if(checkpoint){
				mfrDB$n <- k
				save(mfrDB,file="mfrDB.RData",compress="bzip2")
			}
			if(verbose){
				print(Sys.time()-t2)
			}
		}
	}
	mfrDB$n <- k
	mfrDB
}

degree2DB <- function(mfrDB,N=1000,ns=8:20,maxTime=1200,
                      verbose=TRUE,checkpoint=TRUE)
{
	if(verbose){
	   cat("Running",N,"degree graphs\n")
	}
	t1 <- Sys.time()
	k <- mfrDB$n
	for(n in ns){
		cat(n," ")
		t2 <- Sys.time()
		for(i in 1:N){
			for(d in 2:(n/2)){
				deg <- rep(d,n)
				if((sum(deg) %% 2)==1) deg[1] <- deg[1]+1
				g <- degree.sequence.game(deg,method="vl")
				if(valid4DB(g,maxTime) && is.simple(g) && !is.directed(g)){
					j <- which.DB(g,mfrDB)
					if(is.null(j)){
						mfrDB$timings[[k+1]] <- system.time(mf <- mfr(g,
						            suppress.warning=TRUE))
						if(mf$punted>0){
							k <- k+1
							mfrDB$orders[k] <- vcount(g)
							mfrDB$sizes[k] <- ecount(g)
							mfrDB$graphs[[k]] <- g
							mfrDB$mfrs[[k]] <- mf
						}
					} 
				} else {
				   break
				}
			}
			deg <- sample(2:(n/2),n,replace=TRUE)
			if((sum(deg) %% 2)==1) deg[which.min(deg)] <- deg[which.min(deg)]+1
			g <- degree.sequence.game(deg,method="vl")
			if(valid4DB(g,maxTime) && is.simple(g) && !is.directed(g)){
				j <- which.DB(g,mfrDB)
				if(is.null(j)){
					mfrDB$timings[[k+1]] <- system.time(mf <- mfr(g,
					             suppress.warning=TRUE))
					if(mf$punted>0){
						k <- k+1
						mfrDB$orders[k] <- vcount(g)
						mfrDB$sizes[k] <- ecount(g)
						mfrDB$graphs[[k]] <- g
						mfrDB$mfrs[[k]] <- mf
					}
				} 
			} 
		}
		if(checkpoint){
			mfrDB$n <- k
			save(mfrDB,file="mfrDB.RData",compress="bzip2")
		}
		if(verbose){
			print(Sys.time()-t2)
		}
	}
	mfrDB$n <- k
	mfrDB
}

edges2DB <- function(mfrDB,maxTime=1200,verbose=TRUE,checkpoint=TRUE)
{
	if(verbose){
	   cat("Running edges\n")
	}
	t1 <- Sys.time()
	k <- mfrDB$n
	for(n in 8:20){
		cat(n," ")
		indices <- which(mfrDB$orders==n)
		t2 <- Sys.time()
		for(i in indices){
			if(mfrDB$timings[[i]][['elapsed']]<=maxTime){
				g <- mfrDB$graphs[[i]]
				edges <- get.edgelist(graph.complementer(g))
				for(j in 1:nrow(edges)){
				   h <- add.edges(g,edges[j,])
					if(valid4DB(h,maxTime,checkSplitting=TRUE)){
						a <- which.DB(h,mfrDB)
						if(is.null(a)){
							mfrDB$timings[[k+1]] <- system.time(mf <- mfr(h,
							         suppress.warning=TRUE))
							if(mf$punted>0){
								k <- k+1
								mfrDB$orders[k] <- vcount(h)
								mfrDB$sizes[k] <- ecount(h)
								mfrDB$graphs[[k]] <- h
								mfrDB$mfrs[[k]] <- mf
							}
						}
					}
				}
			}
		}
		if(checkpoint){
			mfrDB$n <- k
			save(mfrDB,file="mfrDB.RData",compress="bzip2")
		}
		if(verbose){
			print(Sys.time()-t2)
		}
	}
	if(verbose){
	   print(Sys.time()-t1)
	}
	mfrDB$n <- k
	mfrDB
}

lines2DB <- function(mfrDB,maxTime=1200,start=1,verbose=TRUE)
{
	if(verbose){
	   cat("Running lines starting at",start,"\n")
	}
	t1 <- Sys.time()
	k <- mfrDB$n
	if(start>k) return(mfrDB)
	for(i in start:length(mfrDB$orders)){
		t2 <- Sys.time()
		h <- mfrDB$graphs[[i]]
		s <- ecount(h)
		if((s>7) && (s<=20)){
			g <- line.graph(h)
			if(valid4DB(g,maxTime,checkSplitting=TRUE)){
				j <- which.DB(g,mfrDB)
				if(is.null(j)){
					mfrDB$timings[[k+1]] <- system.time(mf <- mfr(g,
					           suppress.warning=TRUE))
					if(mf$punted>0){
						k <- k+1
						mfrDB$orders[k] <- vcount(g)
						mfrDB$sizes[k] <- ecount(g)
						mfrDB$graphs[[k]] <- g
						mfrDB$mfrs[[k]] <- mf
					}
				} 
			}
		}
	}
	if(verbose){
	   print(Sys.time()-t1)
	}
	mfrDB$n <- k
	mfrDB
}

compl2DB <- function(mfrDB,maxTime=1200,verbose=TRUE)
{
	if(verbose){
	   cat("Running complementer\n")
	}
	t1 <- Sys.time()
	k <- mfrDB$n+1
	for(i in 1:length(mfrDB$orders)){
		t2 <- Sys.time()
		n <- mfrDB$orders[i]
		if(n>7){
			h <- mfrDB$graphs[[i]]
			g <- graph.complementer(h)
			if(no.clusters(g)>1){
				a <- clusters(g)
				b <- which.max(a$csize)
				g <- subgraph(g,which(a$membership==(b-1))-1)
			}
			if(valid4DB(g,maxTime,checkSplitting=TRUE)){
				j <- which.DB(g,mfrDB)
				if(is.null(j)){
					mfrDB$timings[[k+1]] <- system.time(mf <- mfr(g,
					                        suppress.warning=TRUE))
					if(mf$punted>0){
						k <- k+1
						mfrDB$orders[k] <- vcount(g)
						mfrDB$sizes[k] <- ecount(g)
						mfrDB$graphs[[k]] <- g
						mfrDB$mfrs[[k]] <- mf
					}
				} 
			}
		}
	}
	if(verbose){
	   print(Sys.time()-t1)
	}
	mfrDB$n <- k
	mfrDB
}

unions2DB <- function(mfrDB,maxTime=1200,verbose=TRUE)
{
	if(verbose){
	   cat("Running unions\n")
		t1 <- Sys.time()
	}
   k <- mfrDB$n
	ords <- unlist(lapply(mfrDB$graphs,vcount))
	for(n in min(ords):max(ords)){
	   x <- which(ords==n)
		if(length(x)>1){
		   for(i in 1:(length(x)-1)){
			   g <- mfrDB$graphs[[x[i]]]
				for(j in (i+1):length(x)){
					h <- mfrDB$graphs[[x[j]]]
					k1 <- graph.union(g,h)
					if(valid4DB(k1,maxTime,checkSplitting=TRUE)){
						a <- which.DB(k1,mfrDB)
						if(is.null(a)){
							mfrDB$timings[[k+1]] <- system.time(mf <- mfr(k1,
															suppress.warning=TRUE))
							if(mf$punted>0){
								k <- k+1
								mfrDB$orders[k] <- vcount(k1)
								mfrDB$sizes[k] <- ecount(k1)
								mfrDB$graphs[[k]] <- k1
								mfrDB$mfrs[[k]] <- mf
							}
						} 
					}
					k2 <- removeIsolates(graph.intersection(g,h))
					if(valid4DB(k2,maxTime,checkSplitting=TRUE)){
						a <- which.DB(k2,mfrDB)
						if(is.null(a)){
							mfrDB$timings[[k+1]] <- system.time(mf <- mfr(k2,
															suppress.warning=TRUE))
							if(mf$punted>0){
								k <- k+1
								mfrDB$orders[k] <- vcount(k2)
								mfrDB$sizes[k] <- ecount(k2)
								mfrDB$graphs[[k]] <- k2
								mfrDB$mfrs[[k]] <- mf
							}
						} 
					}
				}
			}
		}
	}
	if(verbose){
	   print(Sys.time()-t1)
	}
	mfrDB$n <- k
	mfrDB
}

products2DB <- function(mfrDB,ps=seq(.1,.3,by=.1),maxTime=1200,verbose=TRUE)
{
	if(verbose){
	   cat("Running products\n")
		t1 <- Sys.time()
	}
   k <- mfrDB$n
	ords <- unlist(lapply(mfrDB$graphs,vcount))
	for(p in ps){
		for(n in 8:19){
			x <- which(ords==n)
			for(m in 1:(20-n)){
				y <- which(ords==m)
				for(i in 1:(length(x)-1)){
					g <- mfrDB$graphs[[x[i]]]
					for(j in (i+1):length(x)){
						h <- mfrDB$graphs[[x[j]]]
						k1 <- product.game(g,h,p=p)
						if(valid4DB(k1,maxTime)){
							a <- which.DB(k1,mfrDB)
							if(is.null(a)){
								mfrDB$timings[[k+1]] <- system.time(mf <- mfr(k1,
																suppress.warning=TRUE))
								if(mf$punted>0){
									k <- k+1
									mfrDB$orders[k] <- vcount(k1)
									mfrDB$sizes[k] <- ecount(k1)
									mfrDB$graphs[[k]] <- k1
									mfrDB$mfrs[[k]] <- mf
								}
							} 
						}
					}
				}
			}
		}
	}
	if(verbose){
	   print(Sys.time()-t1)
	}
	mfrDB$n <- k
	mfrDB
}

create.mfr.database <- function(N=1000,M=20,
										  seed=72368,
										  maxTimeR=1200,
										  maxTimeE=600,
										  maxTimeD=60,
                                reset=FALSE,
										  verbose=TRUE)
{
	set.seed(seed)
	t1 <- Sys.time()
	if(reset){
		mfrDB <- atlas2DB()
		if(verbose){
			cat(length(mfrDB$orders),"\n")
			print(table(mfrDB$orders))
			print(Sys.time()-t1)
		}
		mfrDB <- famous2DB(mfrDB)
		if(verbose){
			cat(length(mfrDB$orders),"\n")
			print(table(mfrDB$orders))
			print(Sys.time()-t1)
		}
		t1 <- Sys.time()
		save(mfrDB,file="mfrDB.RData",compress="bzip2")
		save(mfrDB,file="mfrDBtiny.RData",compress="bzip2")
		mfrDB <- wheels2DB(mfrDB)
		if(verbose){
			cat(length(mfrDB$orders),"\n")
			print(table(mfrDB$orders))
			print(Sys.time()-t1)
		}
		save(mfrDB,file="mfrDB.RData",compress="bzip2")
	} else {
		t1 <- Sys.time()
		if(verbose) cat(length(mfrDB$orders),"\n")
		if(M>=15){
			for(i in 2:10){
				mfrDB <- kidney2DB(mfrDB,ns=15:M,
				                   q=i/10,N=N,verbose=verbose,
										 checkpoint=TRUE,maxTime=maxTimeR)
				save(mfrDB,file="mfrDB.RData",compress="bzip2")
				if(verbose){
					cat(length(mfrDB$orders),"\n")
					print(table(mfrDB$orders))
					print(Sys.time()-t1)
				}
				mfrDB <- er2DB(mfrDB,N=N,ns=8:M,verbose=verbose,
									p=i/50+.05,
									checkpoint=TRUE,maxTime=maxTimeR)
				save(mfrDB,file="mfrDB.RData",compress="bzip2")
				if(verbose){
					cat(length(mfrDB$orders),"\n")
					print(table(mfrDB$orders))
					print(Sys.time()-t1)
				}
			}
		}
		t1 <- Sys.time()
		mfrDB <- star2DB(mfrDB,N=N,ns=8:M,verbose=verbose,
		                 checkpoint=TRUE,maxTime=maxTimeR)
		save(mfrDB,file="mfrDB.RData",compress="bzip2")
		if(verbose){
			cat(length(mfrDB$orders),"\n")
			print(table(mfrDB$orders))
			print(Sys.time()-t1)
		}
		for(i in 1:10){
		mfrDB <- grg2DB(mfrDB,N=N,ns=8:M,verbose=verbose,r=i/25,
		                checkpoint=TRUE,maxTime=maxTimeR)
		}
		save(mfrDB,file="mfrDB.RData",compress="bzip2")
		if(verbose){
			cat(length(mfrDB$orders),"\n")
			print(table(mfrDB$orders))
			print(Sys.time()-t1)
		}
		mfrDB <- degree2DB(mfrDB,N=N,ns=8:M,maxTime=maxTimeD,
		                   verbose=verbose,checkpoint=TRUE)
		save(mfrDB,file="mfrDB.RData",compress="bzip2")
		if(verbose){
			cat(length(mfrDB$orders),"\n")
			print(table(mfrDB$orders))
			print(Sys.time()-t1)
		}
		start <- mfrDB$n+1
		mfrDB <- lines2DB(mfrDB,maxTime=maxTimeR,verbose=verbose)
		save(mfrDB,file="mfrDB.RData",compress="bzip2")
		if(verbose){
			cat(length(mfrDB$orders),"\n")
			print(table(mfrDB$orders))
			print(Sys.time()-t1)
		}
		mfrDB <- edges2DB(mfrDB,maxTime=maxTimeE,verbose=verbose,
		                  checkpoint=TRUE)
		save(mfrDB,file="mfrDB.RData",compress="bzip2")
		if(verbose){
			cat(length(mfrDB$orders),"\n")
			print(table(mfrDB$orders))
			print(Sys.time()-t1)
		}
		mfrDB <- lines2DB(mfrDB,maxTime=maxTimeR,start=start,verbose=verbose)
		save(mfrDB,file="mfrDB.RData",compress="bzip2")
		if(verbose){
			cat(length(mfrDB$orders),"\n")
			print(table(mfrDB$orders))
			print(Sys.time()-t1)
		}
		mfrDB <- compl2DB(mfrDB,maxTime=maxTimeE,verbose=TRUE)
		save(mfrDB,file="mfrDB.RData",compress="bzip2")
		if(verbose){
			cat(length(mfrDB$orders),"\n")
			print(table(mfrDB$orders))
			print(Sys.time()-t1)
		}
		mfrDB <- unions2DB(mfrDB,maxTime=maxTimeR,verbose=TRUE)
		save(mfrDB,file="mfrDB.RData",compress="bzip2")
		if(verbose){
			cat(length(mfrDB$orders),"\n")
			print(table(mfrDB$orders))
			print(Sys.time()-t1)
		}
		mfrDB <- products2DB(mfrDB,maxTime=maxTimeR,verbose=TRUE)
		save(mfrDB,file="mfrDB.RData",compress="bzip2")
		if(verbose){
			cat(length(mfrDB$orders),"\n")
			print(table(mfrDB$orders))
			print(Sys.time()-t1)
		}
	}
}


checkDB1 <- function(z)
{
	s <- length(z)/2
	e1 <- z[(1:s)]
	e2 <- z[-(1:s)]
   g <- graph(rbind(e1,e2),directed=FALSE)
	m <- checkDB(g)
	if(is.null(m)) return(as.integer(0))
	as.integer(c(m$pd,m$reg,c(t(m$graded))))
}

testDB <- function(g)
{
   n <- vcount(g)
	s <- ecount(g)

	graded <- rep(0,n*n)

	edges <- get.edgelist(g)
	punted <- 0
	retval <- .C("TESTDB",
					 edges1 = as.integer(edges[,1]),
					 edges2 = as.integer(edges[,2]),
					 N = as.integer(n),
					 S = as.integer(s),
					 graded = as.double(graded),
					 pd = integer(1),
					 reg = integer(1),
					 found = integer(1),
					 PACKAGE="mfr")
	 if(retval$found){
		 graded <- matrix(retval$graded[1:(retval$reg*(retval$pd+1))],
							byrow=TRUE,
	                  nrow=retval$reg,ncol=retval$pd+1)
	 }
	 else{
	    return(NULL)
	 }
	bettis <- apply(graded,2,sum)
	graded <- graded[,bettis>0]
	bettis <- bettis[bettis>0]
	 list(bettis=bettis,graded=graded,reg=nrow(graded),pd=ncol(graded)-1,
	      punted=retval$punted)
}

combineDBs <- function(db1,db2,verbose=FALSE)
{
	k <- length(db1$orders)
   n <- length(db2$orders)
	if(n>k) { # swap the databases so the smaller is second
	   a <- db1
		db1 <- db2
		db2 <- a
		a <- k
		k <- n
		n <- a
	}
	k <- k+1
	if(verbose){
	   cat("Combining a database of",n,"graphs with one of",k-1,"graphs\n")
	}
	graphs <- db2$graphs
	for(i in 1:n){
		g <- graphs[[i]]
		if(is.null(which.DB(g,db1))){
			db1$orders[k] <- db2$orders[i]
			db1$sizes[k] <- db2$sizes[i]
			db1$graphs[[k]] <- g
			db1$timings[[k]] <- db2$timings[i]
			db1$mfrs[[k]] <- db2$mfrs[[i]]
			k <- k+1
		}
		if(verbose){
		   if((i %% 1000)==0){
			   cat(i,length(db1$orders),"\n")
			}
		}
	}
	db1$n <- length(db1$orders)
	db1
}
