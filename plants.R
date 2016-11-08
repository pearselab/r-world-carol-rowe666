 setup.plants <- function(repro, survive, comp.mat, names=NULL){
   if(is.null(names)){
    names <- letters[seq_along(repro)]
   }
   if(length(names) < length(repro)){
     stop("You didn't enter the correct number of species")
   }
  if(length(repro) != length(survive)){
    stop("Reproduction and survival parameters needed for all species")
  }
   if(any(repro > 1) | any(repro < 0)){
     stop("Probabilities out of range in repro vector")
  }
   if(any(repro > 1) | any(repro < 0)){
     stop("Probabilities out of range in survive vector")
   }
   if(any(repro > 1) | any(repro < 0)){
     stop("Probabilities out of range in competition matrix")
   }
  repro <- setNames(repro, names)
  survive <- setNames(survive, names)
  #...what does the line above do? Do you want more like it?
  return(list(repro=repro, survive=survive, comp.mat=comp.mat,
              names=names))
}

 
repro <- runif(n=4, min=0, max=1)
print(repro)
survive <- runif(n=length(repro), min=0, max=1)
comps <- runif(n=length(repro), min=0, max=1)
comps <- runif(n=length(repro)^2, min = 0, max = 1)
comp.mat <- matrix(comps, nrow = length(repro), ncol = length(repro))
names <- list('Betula', 'Popoulus', 'Pteridium', 'Pseudostuga')
info <- setup.plants(repro, survive, comp.mat, names)
info
cat("testing to see what's going on:", survive, "\n")

#Tested this function by substituting "Betula" for plants. It works.
survive <- function(cell, info){
  if(cell == "NA" | cell == ''){
    return(cell)
  }
  if(runif(1) <= info$survive[plants]){
    # [[plant]] if you want just the numeric value without the name
    cell <- info$survive[plants]
    return(cell)
  }
}


plant.timestep <- function(plants, terrain, info){
  survive <- function(plant, info){
    #...survive function...
  }
  #...looping et al...
  return(new.plants.matrix)
} 


