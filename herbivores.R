#create a class for one species of herbivores
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

eat <- c(1,2,3,4,5)

stegosaurus <- setup.herbivores(eat,0.5,0.2)
