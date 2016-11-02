#1: create matrix with odd dimensions
#2: initialize random values for each corner

terrain <- matrix(nrow = 7, ncol = 7)

terrain[1,1] <- rnorm(1, rnorm(1), runif(1, min=0))
terrain[1,7] <- rnorm(1, rnorm(1), runif(1, min=0))
terrain[7,1] <- rnorm(1, rnorm(1), runif(1, min=0))
terrain[7,7] <- rnorm(1, rnorm(1), runif(1, min=0))

#3: create diamond f(x)

#4: create square f(x)

#5: f(x) to fill matrix
