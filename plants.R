#' Creates plant class and loops through their generations over time
#' @param setup.plants takes reproduction, survival, and competition parameters for our plant species
#' @param survive determines if the plant survives (at a given timestep)
#' @param run.plant.ecosystem loops through the plant matrix and determines if they live, die,
#'    or reproduce at a each timestep; uses a terrain matrix from terrain.R
#' @param reproduce determines if a plant reproduces (if it survives)
#' @examples 
#' #inputs for the setup.plants function/class
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
#' plants <- setup.plants(repro,survival,comp.mat)
#' eco <- run.plant.ecosystem(plants, terrain)
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

##Survive function to determine if the plant survives (at a given timestep)
survive <- function(cell, plants){
  #checks if a cell is NA or under water (< 0) and not ""
  if ((is.na(cell) | cell < 0) & cell != ""){
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

#Ecosystem through time
run.plant.ecosystem <- function(plants,terrain,timestep=1){
  #create a plant array to keep track of the plant matrix over time
  plant.pop <- array("", dim=c(dim(terrain),timestep+1))
  #initital plant population
  inds <- c("",plants$names)
  plant.pop[1,1:ncol(plant.pop),1] <- sample(inds[1:length(inds)],ncol(plant.pop),replace=T)
  plant.pop[2,1:ncol(plant.pop),1] <- sample(inds[1:length(inds)],ncol(plant.pop),replace=T)
  plant.pop[3,1:ncol(plant.pop),1] <- sample(inds[1:length(inds)],ncol(plant.pop),replace=T)
  plant.pop[4,1:ncol(plant.pop),1] <- sample(inds[1:length(inds)],ncol(plant.pop),replace=T)
  plant.pop[5,1:ncol(plant.pop),1] <- sample(inds[1:length(inds)],ncol(plant.pop),replace=T)
  #loop through time, columns, rows, then adds a timestep
  for(i in seq_len(dim(plant.pop)[3])){
    for(j in 1:(dim(plant.pop)[2])){
      for(k in 1:(dim(plant.pop)[1])){
        plant.pop[k,j,i] <- survive(plant.pop[k,j,i],plants)
        timestep <- timestep + 1
      }
    }
    #makes any plants generated in water NA
    if(plant.pop[,,i][is.na(terrain)]){
      cell <- NA
    }
    #breaks the loop after 100 timesteps
    if(timestep == 100){
      break
    }
  }
  return(plant.pop)
}

#don't forget to reproduce
#plant <- reproduce(row, column, plants, info)

#reproduce <- function(row, col, plants, info){
 # possible.locations <- as.matrix(expand.grid(row+c(-1,0,1), col+c(-1,0,1))) 
  #filter out which ones are not water-logged and reproduce there 
  
  #...being careful to check you do have somewhere to reproduce to!... return(plants)
#}
