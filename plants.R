#function that takes as input reproduction, survival, and competition parameters for our plant species. 
#The program will check that both the reproduction and survival vectors are the same length (the number
#of species in our simulation), and that the competition argument is a matrix with dimensions equal to 
#the length of the other variables (this matrix determines the likelihood that each species will will 
#when in competition with another). You’re going to use the stop function to ‘raise’ an error if those 
#conditions aren’t met

#class for setting up plants to use
setup.plants <- function(repro, survival, comp.mat, names=NULL){
  #if the user doesnt give me names, make names be a,b,c.. for as many entries as there are in repro
  if(is.null(names))
    names <- letters[seq_along(repro)]
  #survival[] names match matrix, subset, surv and repro for the matrix
  
  if(length(repro) != length(survival))
    stop("Reproduction and survival parameters needed for all species")
  #more tests...which?
  
  #give names for each variable for each plant
  repro <- setNames(repro, names)
  surv <- setNames(survival, names)
  comp.mat <- setNames(comp.mat, names)
  #defining the class
  output <- list(weight=repro, survival=survival, comp.mat= comp.mat, names=names)
  class(output) <- "plants"
  return(output)
}

#inputs for the setup.plants function/class
repro <- c(0.2,0.4,0.6)
survival <- c(0.8,0.6,0.2)
comp.mat <- matrix(data=NA, nrow=3, ncol=3)
comp.mat[1,1] <- 0.5
comp.mat[1,2] <- 0.3
comp.mat[1,3] <- 0.8
comp.mat[2,1] <- 0.3
comp.mat[2,2] <- 0.5
comp.mat[2,3] <- 0.1
comp.mat[3,1] <- 0.8
comp.mat[3,2] <- 0.1
comp.mat[3,3] <- 0.5
