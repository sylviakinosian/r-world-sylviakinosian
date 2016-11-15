## Setup plants function
#function/class that takes as input reproduction, survival, and competition parameters for our plant species. 
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

##Survive function to determine if the plant survives (at a given timestep)
survive <- function(cell, plants){
  #checks if a cell is NA or is under water (< 0)
  if (is.na(cell) | cell < 0){
    cell <- NA
  }else{
    #compares a random value (0:1) with the survival probablity fo each plant
    if(runif(1) <= plants$survival[cell]){
      cell <- cell
    }else{
      cell <- ""
    }
  }
  return(cell)
}

#Timestep  - plants through time
#set up to work with the terrain matrix from "terrain.R"
#looping acrossc columns and rows
plant.timestep <- function(plants, terrain){
  new.plants.matrix <- terrain
  apply(new.plants.matrix, 1, survive)
  return(new.plants.matrix)
}

# i don't want it .. i don't need it


#Ecosystem through time
run.plant.ecosystem <- function(plants,terrain,timestep=10){
  #create a plant array to keep track of the plant matrix over time
  plant.pop <- array("", dim=c(dim(terrain),timestep))
  #initital plant population
  inds <- c("",plants$names)
  plant.pop[1,1:ncol(plant.pop),1] <- sample(inds[1:length(inds)],ncol(plant.pop),replace=T)
  plant.pop[2,1:ncol(plant.pop),1] <- sample(inds[1:length(inds)],ncol(plant.pop),replace=T)
  plant.pop[3,1:ncol(plant.pop),1] <- sample(inds[1:length(inds)],ncol(plant.pop),replace=T)
  plant.pop[4,1:ncol(plant.pop),1] <- sample(inds[1:length(inds)],ncol(plant.pop),replace=T)
  plant.pop[5,1:ncol(plant.pop),1] <- sample(inds[1:length(inds)],ncol(plant.pop),replace=T)
  #loop through rows, column, for each timestep
  for(i in seq_len(dim(plant.pop)[3])){
    for(j in seq_len(dim(plant.pop)[2])){
      for(k in seq_len(dim(plant.pop)[1])){
        survive(plant.pop[k,j,i],plants)
        timestep <- timestep + 1
      }
    }
    #if(plant.pop[,,i][is.na(terrain)]){
    #  cell <- NA
    #}
    if(timestep == 100){
      break
    }
  }
  return(plant.pop)
}

#don't forget to reproduce
plant <- reproduce(row, column, plants, info)

reproduce <- function(row, col, plants, info){
  possible.locations <- as.matrix(expand.grid(row+c(-1,0,1), col+c(-1,0,1))) 
  #...now filter out which ones are not water-logged and reproduce there... 
  #...being careful to check you do have somewhere to reproduce to!... return(plants)
}
