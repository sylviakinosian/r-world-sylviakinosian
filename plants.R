#function that takes as input reproduction, survival, and competition parameters for our plant species. 
#The program will check that both the reproduction and survival vectors are the same length (the number
#of species in our simulation), and that the competition argument is a matrix with dimensions equal to 
#the length of the other variables (this matrix determines the likelihood that each species will will 
#when in competition with another). You’re going to use the stop function to ‘raise’ an error if those 
#conditions aren’t met

setup.plants <- function(repro, survival, comp.mat, names=NULL){
  if(is.null(names))
    names <- letters[seq_along(repro)]
  
  if(length(repro) != length(survive))
    stop("Reproduction and survival parameters needed for all species")
  
  repro <- setNames(repro, names)
  output <- list(weight=repro, survival=survival, comp.mat= comp.mat, names=names)
  class(output) <- "plants"
  return(output)
}