#' Makes an elevational grid, denotes cells with an elevation of less than 1 as water
#' @param odd.matrix creates matrix with odd dimensions
#' @param diamond.step fills in the center value of a matrix (or subset) by averaging the four corners
#' @param square.step fills in the side values of a matrix (or subset) by averaging the corners and middle
#' @param dia.sq.step applies diamond.step and square.step to a matrix
#' @param make.terrain akes terrain created by dia.sq.step and changes values < 0 to NA (water)
#' @examples 
#' terrain <- odd.matrix(5)
#' terrain <- diamond.step(terrain)
#' terrain <- square.step(terrain)
#' terrain <- dia.sq.step(5)
#' terrain <- make.terrain(terrain)
#' image(terrain)
#' @export

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
terrain <- odd.matrix(5)

# create diamond step f(x)
diamond.step <- function(matrix){
	#find corners - top/bottom, left/right
  tL <- matrix[1,1]
  tR <- matrix[1,ncol(matrix)]
  bL <- matrix[nrow(matrix),1]
  bR <- matrix[nrow(matrix),ncol(matrix)]
  #avg value for center
  c <- jitter(mean(tL,tR,bL,bR))
  #find center, assign new value
  matrix[mean(1:nrow(matrix)),mean(1:ncol(matrix))] <- c
  return(matrix)
}
terrain <- diamond.step(terrain)

#4: create square f(x)
square.step <- function(matrix){
  #na.rm=TRUE ??
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
terrain <- square.step(terrain)

#5: f(x) to fill matrix - takes dimensions, seeds, and updates
#set up for a 5 by 5 matrix - will improve later
#for looping: seq(1,5,4) -> seq(1,5,2) .. subsetting matrix
dia.sq.step <- function(x){
  mat <- odd.matrix(x)
  mat <- diamond.step(mat)
  mat <- square.step(mat)
  # for upper left quadrant of matrix
  #could also have used dim(mat)[1] // dim(mat[2])
  uL <- mat[1:mean(1:nrow(mat)),1:mean(1:ncol(mat))]
  uL <- diamond.step(uL)
  uL <- square.step(uL)
  mat[1:mean(1:nrow(mat)),1:mean(1:ncol(mat))] <- uL
  # for upper right
  uR <- mat[1:mean(1:nrow(mat)),mean(1:ncol(mat)):ncol(mat)]
  uR <- diamond.step(uR)
  uR <- square.step(uR)
  mat[1:mean(1:nrow(mat)),mean(1:ncol(mat)):ncol(mat)] <- uR
  # for lower left
  lL <- mat[mean(1:nrow(mat)):row(mat),1:mean(1:ncol(mat))]
  lL <- diamond.step(lL)
  lL <- square.step(lL)
  mat[mean(1:nrow(mat)):nrow(mat),1:mean(1:ncol(mat))] <- lL
  #for lower right
  lR <- mat[mean(1:nrow(mat)):nrow(mat),mean(1:ncol(mat)):ncol(mat)]
  lR <- diamond.step(lR)
  lR <- square.step(lR)
  mat[mean(1:nrow(mat)):nrow(mat),mean(1:ncol(mat)):ncol(mat)] <- lR
  #write something so it stops when everything is filled
  print(mat)
  return(mat)
}

terrain <- dia.sq.step(5)

# function that takes terrain, changes values < 0 to NA (water)
make.terrain <- function(matrix){
  if (!is.matrix(matrix))
    stop("Need a matrix!")
  matrix[terrain < 0] <- NA
  return(matrix)
}

terrain <- make.terrain(terrain)
image(terrain)
