myfun1 <- function(x, gr1, gr2, num){
	x[,gr1] <- as.factor(x[,gr1])
	x[,gr2] <- as.factor(x[,gr2])
	11 <- length(levels(x[,gr1]))
	12 <- length(levels(x[,gr1]))
	gr <- numeric(11*12)
	c1 <- c2 <- character(11*12) 
	ii <- jj <- 0 
	for(i in levels(x[,gr1])){
		for(j in levels(x[,gr2])){
			ii <- ii +1
			c1[ii] <- i 
			c2[ii] <- j 
			vec <- x[x[,gr2] == j & x[,gr1] == i, num]
			if(length(vec) > 0) gr[ii] <- meanFor(vec)
		}
	}
	df <- data.frame 
}