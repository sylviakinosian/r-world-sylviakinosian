#create matrix with odd dimensions

odd.matrix<- function(x){

	#if x is an even number, f(x) adds 1 to make it odd
	if (x %% 2 == 0){
	x <- x+1
	}

	#create a matrix with odd dimensions		
	mat  <- matrix(nrow = x, ncol = x)
	
	#initilize random values for each corner
	mat[1, 1] <- rnorm(1, rnorm(1), runif(1, min=0))
	mat[1, ncol(mat)] <- rnorm(1, rnorm(1), runif(1, min=0))
	mat[nrow(mat), 1] <- rnorm(1, rnorm(1), runif(1, min=0))
	mat[nrow(mat), ncol(mat)] <- rnorm(1, rnorm(1), runif(1, min=0))
	
	return(mat)
}

# create diamond step f(x)

diamond.step <- function(x){
	m <- mat(x)
	

}

#4: create square f(x)

square.step <- function(){
}

#5: f(x) to fill matrix
