#' Creates a plant class with values for reproduction, survival, and competition
#' @param repro takes a list of values (probabilities) that a given plant species will reproduce
#' @param survival take a list of values (probabilities) that a given plant species will survive
#' @param comp.mat take a matrix values values (probabilites) for how two plants (of the same of different species) will interact with each other
#' @param names takes a list of names for the different plant species, defaults to "a, b, c" 
#' @examples 
#' repro <- c(0.2,0.4,0.6)
#' survival <- c(0.8,0.6,0.2)
#' comp.mat <- matrix(data=NA, nrow=3, ncol=3, dimnames = list(c("a","b","c"),c("a","b","c")))
#' comp.mat[1,1] <- 0.5
#' comp.mat[1,2] <- 0.3
#' comp.mat[1,3] <- 0.8
#' comp.mat[2,1] <- 0.3
#' comp.mat[2,2] <- 0.5
#' comp.mat[2,3] <- 0.1
#' comp.mat[3,1] <- 0.8
#' comp.mat[3,2] <- 0.1
#' comp.mat[3,3] <- 0.5
#' fernz <- setup.plants(repro, survival, comp.mat)
#' @export

## Setup plants function
#function/class that takes as input reproduction, survival, and competition parameters for our plant species. 
setup.plants <- function(repro, survival, comp.mat, names=NULL){
  #if the user doesnt give names, make names be a,b,c..
  if(is.null(names))
    names <- letters[seq_along(repro)]
  if(length(repro) != length(survival))
    stop("Reproduction and survival parameters needed for all species")
  if(!is.matrix(comp.mat))
    stop("Need competition probabilities in a matrix!")
  #give names for each variable for each plant
  repro <- setNames(repro, names)
  survival <- setNames(survival, names)
  #defining the class
  output <- list(repro=repro, survival=survival, comp.mat= comp.mat, names=names)
  class(output) <- "plants"
  return(output)
}

#inputs for the setup.plants function/class
repro <- c(0.2,0.4,0.6)
survival <- c(0.8,0.6,0.2)
comp.mat <- matrix(data=NA, nrow=3, ncol=3, dimnames = list(c("a","b","c"),c("a","b","c")))
comp.mat[1,1] <- 0.5
comp.mat[1,2] <- 0.3
comp.mat[1,3] <- 0.8
comp.mat[2,1] <- 0.3
comp.mat[2,2] <- 0.5
comp.mat[2,3] <- 0.1
comp.mat[3,1] <- 0.8
comp.mat[3,2] <- 0.1
comp.mat[3,3] <- 0.5

plants <- setup.plants(repro,survival,comp.mat)

