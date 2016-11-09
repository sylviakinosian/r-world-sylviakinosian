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
terrain <- odd.matrix(7)

# create diamond step f(x)
diamond.step <- function(matrix){
	#find corners - top/bottom, left/right
  tL <- matrix[1,1]
  tR <- matrix[1,ncol(matrix)]
  bL <- matrix[nrow(matrix),1]
  bR <- matrix[nrow(matrix),ncol(matrix)]
  #avg value for center
  c <- jitter(mean(tL,tR,bL,bR))
  #find center
  matrix[mean(1:nrow(matrix)),mean(1:ncol(matrix))] <- c
  return(matrix)
}
diamond.step(terrain)

#4: create square f(x)
square.step <- function(matrix){
  #find corners (top/bottom, left/right) and center
  tL <- matrix[1,1]
  tR <- matrix[1,ncol(matrix)]
  bL <- matrix[nrow(matrix),1]
  bR <- matrix[nrow(matrix),ncol(matrix)]
  c <- matrix[mean(1:nrow(matrix)),mean(1:ncol(matrix))]
  #new cell values to be filled in
  t <- jitter(mean(tL,tR,c))
  b <- jitter(mean(bL,bR,c))
  l <- jitter(mean(tL,bL,c))
  r <- jitter(mean(tR,bR,c))
  #fill em in
  matrix[1,mean(1:ncol(matrix))] <- t
  matrix[nrow(matrix),mean(1:ncol(matrix))] <- b
  matrix[mean(1:nrow(matrix)),1] <- l
  matrix[mean(1:nrow(matrix)),ncol(matrix)] <- r
  return(matrix)
}
square.step(terrain)

#5: f(x) to fill matrix
dia.sq.step <- function(matrix){
  diamond.step(matrix)
  square.step(matrix)
  #if(!na){
  #  return(matrix)
  #}
  return(matrix)
}
