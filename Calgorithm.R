 #function to calculate the fit of a cluster output against a factor result
 # n<-120
 # x= as.factor(sample(c('a', 'b', 'c', 'd'), n, replace=T))
 # y= sample(1:4, n, replace=T)
 library(combinat)
 # scores=numeric(fact(4))

 # for (i in 1:fact(4)){
 	# levels(x) <- permn(4)[[i]]
 	# scores[i] <- length(which(x==y))
 # }
 
calgorithm <- function(x, y) {
 	if (length(x) != length(y)) {
 		stop("Vector lengths do not match")
 	}
 	n <- length(y)
 	v <- length(unique(y))
 	scores=numeric(fact(v))
 	y <- as.numeric(y)
 	x <- as.factor(x)
 	for (i in 1:fact(v)){
 		levels(x) <- permn(v)[[i]]
 		scores[i] <- length(which(x==y))
 	}
 	return(max(scores)/n)
 }