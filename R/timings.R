timings <- function(N=100,ns=10:20,ps=.1)
{
   x <- vector("list",choose(max(ns),2))
	for(i in 1:N){
	   for(n in ns){
			for(p in ps){
				g <- erdos.renyi.game(n,p)
				s <- ecount(g)
				if(s>0){
					a <- system.time(singular(g))['elapsed']
					x[[s]] <- c(x[[s]],a)
					print(a)
				}
			}
		}
	}
	names <- which(unlist(lapply(x,length))>0)
	list(x[names],names)
}

fit.timings <- function(z,from=10,by.max=TRUE)
{
   if(missing(z)) z <- timings()
	data <- z[[1]]
	names <- z[[2]]
	boxplot(data,names=names,log="y")
	a <- which(names>=from)
	data <- data[a]
	names <- names[a]
   x <- NULL
	y <- NULL
	for(i in 1:length(data)){
		if(by.max){
		   x <- c(x,names[i])
			y <- c(y,max(data[[i]]))
		} else {
			x <- c(x,rep(names[i],length(data[[i]])))
			y <- c(y,data[[i]])
		}
	}
	l <- lm(log(y)~x)
	co <- coefficients(l)
	lines(names,exp(names*co[2]+co[1]))
	l
}
