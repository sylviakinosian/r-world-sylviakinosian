#1: create matrix with odd dimensions

odd.matrix<- function(x){

	#if x is an even number, f(x) adds 1 to make it odd
	if (x %% 2 == 0){
	x <- x+1
	}

	#create a matrix with odd dimensions		
	m  <- matrix(nrow = x, ncol = x)

	return(m)
	kalimdor <- m
}

kalimdor <- odd.matrix(4)

#2: initilize random values for each corner

kalimdor[1, 1] <- rnorm(1, rnorm(1), runif(1, min=0))
kalimdor[1, ncol(kalimdor)] <- rnorm(1, rnorm(1), runif(1, min=0))
kalimdor[nrow(kalimdor), 1] <- rnorm(1, rnorm(1), runif(1, min=0))
kalimdor[nrow(kalimdor), ncol(kalimdor)] <- rnorm(1, rnorm(1), runif(1, min=0))

#3: create diamond f(x)

diamond.step <- function(){
}

#4: create square f(x)

square.step <- function(){
}

#5: f(x) to fill matrix
