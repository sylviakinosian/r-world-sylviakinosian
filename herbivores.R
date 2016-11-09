#create herbivores
setup.herbivores <- function(eat, kill, repro){
  if(!is.numeric(eat) | !is.numeric(kill) | !is.numeric(repro))
      stop("Herbivores need dat numeric data")
  if(length(kill) != 1)
      stop("dem herbs need one kill probability")
  if(length(repro) != 1)
      stop("Dem need one reproduction probability!!!1!!1")
  sated <- length(eat)
  output <- (list(eat=eat, kill=kill, repro=repro, sated=sated))
  class(output) <- "herbivores"
  return(output)
}

#herbivores move, but can't move to a cell with NA or another herbivore present
new.loc <- function(){
  
}

#eating
#runif(to reproduce)
#runif(to kill ... need to edit plant matrix)

herbivore.timestep
#loops everything over time
#nesting turtles