#function/class that takes as input reproduction, survival, 
#and competition parameters for our plant species. 
setup.plants <- function(repro, survival, comp.mat, names=NULL){
  #if the user doesnt give names, make names be a,b,c..
  if(is.null(names))
    names <- letters[seq_along(repro)]
  if(length(repro) != length(survival))
    stop("Reproduction and survival parameters needed for all species")
  #more tests...which?
  if(!is.matrix(comp.mat))
    stop("Need competition probabilities in a matrix!")
  #give names for each variable for each plant
  repro <- setNames(repro, names)
  surv <- setNames(survival, names)
  #defining the class
  output <- list(weight=repro, survival=survival, comp.mat= comp.mat, names=names)
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

fernz <- setup.plants(repro,survival,comp.mat)

#function to determine if the plant survives
survive <- function(cell, info){
  #code to check whether cell is empty or has water
  #if(terrain[] = -number(water))
  #{no plants can grow here}
  #if(no plants)
  #{yay}
  #if(plant present already)
  #{compete}
  if(runif(1) <= info$survive[plant])
    ####Do you understand why comparing that with a probability helps us draw something with that probability?
  #$The plant survived! so do something...
}

#plants through time
plant.timestep <- function(plants, terrain, info){
  survive <- function(plant, info){
    #...survive function...
  }
  #...looping et al...
  return(new.plants.matrix)
}

#array??
plants <- array("", dim=c(dim(terrain),timesteps+1)) 
#...why timesteps+1, do you think?... because we start at t=1!
for(i in seq_len(dim(plants)[3]))
  plants[,,i][is.na(terrain)] <- NA

#don't forget to reproduce
plant <- reproduce(row, column, plants, info)

reproduce <- function(row, col, plants, info){
  possible.locations <- as.matrix(expand.grid(row+c(-1,0,1), col+c(-1,0,1))) 
  #...now filter out which ones are not water-logged and reproduce there... 
  #...being careful to check you do have somewhere to reproduce to!... return(plants)
}
