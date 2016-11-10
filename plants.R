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
survive <- c(0.1, 0.3, 0.6, 0.8)
comps <- runif(n=length(repro), min=0, max=1)
comps <- runif(n=length(repro)^2, min = 0, max = 1)
comp.mat <- matrix(comps, nrow = length(repro), ncol = length(repro))
names <- list('Betula', 'Popoulus', 'Pteridium', 'Pseudotsuga')

# Not sure if I'm supposed to reassign.
# Can anyone find a good tutorial on setNames? Know what it's doing, but not how to utilize the output.
info <- setup.plants(repro, survive, comp.mat, names)
names <- info$names
repro <- info$repro
survive <- info$survive
comp.mat <- info$comp.mat



#Tested this function by substituting "Betula" for names. It seems to work.
# survive: if cell is NA or less than 0, then leave as is.
# if survives, then give cell the plant name
# if doesn't survive, then make the cell empty
survive <- function(cell, info){
  if(is.na(cell) | cell < 0){
    cell <- cell
  }
  if(runif(1) <= info$survive[names]){
    names <- info$survive[names]
    # [[names]] if you want just the numeric value without the name
    cell <- names
  }else{
    cell <- ""
  }
return(cell)
}


# terrain is matrix I have called my.world
plant.timestep <- function(my.world, info){
  new.plants.matrix <- matrix(data = NA, nrow= nrow(my.world), ncol=ncol(my.world))
  for(i in 1:nrow(my.world)){
    for(j in 1:ncol(my.world)){
      newplant <- survive(my.world[i,j], info)
      cat("newplant is:", newplant, "\n")
      new.plants.matrix[i,j] <- newplant
    }
  }
  return(new.plants.matrix)
}




## creating a dummy matrix for my.world
## woould be nice to test to see if function works
data <- rnorm(10, 1, 20)
my.world <- matrix(data =data, nrow = 5, ncol = 5)
my.world
new.plants.matrix <- plant.timestep(my.world, info)
new.plants.matrix

# plants <- array("", dim=c(dim(terrain),timesteps+1))
# #...why timesteps+1, do you think?... Because you are entering info into the next timestep!
# for(i in seq_len(dim(plants)[3]))
#   plants[,,i][is.na(terrain)] <- NA

