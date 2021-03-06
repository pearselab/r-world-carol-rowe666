## Anyone looking at this.....does my logic make sense???
## Have NOT worked on the plants array yet...that's where I left off.

# Items needed from the terrain.R: my.world, size (which is numrows(my.world))
# Items needed from user for plants.R: names, repro, survive, comp.mat, init.plant.amt

######## SETUP.PLANTS FUNCTION ################
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
  return(list(repro=repro, survive=survive, comp.mat=comp.mat,
              names=names))
}

###### END OF SETUP.PLANTS FUNCTION ############

## setting up some dummies to play with for testing
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
# names <- info$names
# repro <- info$repro
# survive <- info$survive
# comp.mat <- info$comp.mat

# repro[1] # this gives me both Betula AND the repro number
# repro[[1]] or repro["Betula"] # either of these gives me JUST the repro number
### LOOKY HERE! TOOK ME FOREVER TO FIGURE THIS SIMPLE THING OUT TO RETURN THE NAME!
# names(repro[1]) # AH HA!! this gives me JUST the name of the plant!!! This took LOTS of playing to figure out!!!

#Tested this function by substituting "Betula" for names. It seems to work.
# survive: if cell is NA or less than 0, then leave as is.
# if survives, then give cell the plant name
# if doesn't survive, then make the cell empty
## And jsut WHAT is CELL???? is that [i,j] from what??? Or, am I putting something INTO the cell? THen why are
## you calling cell in the function: function(cell, info), rather, I'd do function(info) and then return(cell)
#### Will, you've got me VERY confused.
## for now, I have cell going in and cell going out.....
## Update on comments: I think that cell is from terrain (my.world) matrix to see if we can even put a plant there.
## So, my thinking is that we return "cell" such that the new matrix created in plant.timestep will retain the NA, the '', or the plant name.

#########   SURVIVE FUNCTION ###############
survive <- function(cell, info){
  if(is.na(cell) | cell < 0){
    cell <- cell
  }
  # this makes it easier for me
  survive <- info$survive
  names <- info$names
  num <- length(names)
  # I am going to assume that this survive function will be called within a loop, such that i will be defined.
  x <- sample(1:num, 1, replace = TRUE)
  if(runif(1) <= survive[[x]]){ #just probability form the info$survive at location i
    plant <- names(survive[x])
    cell <- plant
  }else{
    cell <- ""
  }
return(cell)
}
###########  END OF SRUVIVE FUNCTION ##############

## Passing some dummy values to test the survive function as best I can; looks like it's working 
# x <- 1
#cell <- 0.2
#survive(cell, info)

####### NOTE: moved plant.timestep to end of this file. It appears that all is going into this function####
##########################################################################################################

# ## creating a dummy matrix for my.world
# ## woould be nice to test to see if plant.timestep function works
# data <- rnorm(10, 1, 20)
# my.world <- matrix(data =data, nrow = 5, ncol = 5)
# my.world
# 
# # now testing the new.plants.matrix -- something looks like it's happening/working? Shocking, indeed!
# x <- 1
# new.plants.matrix <- plant.timestep(my.world, info)
# new.plants.matrix


############## JUST PLAYING WITH ARRAYS - THIS WILL GET TOSSED ##################
########## now I have to see what arrays are about and what Will is doing in the plant array skeleton he provided:
# Create two vectors.
vector1 <- c(1,2,3,4,4,6,7,8,9)
vector2 <- c(11,12,13,14,15,16,17,18,19)

# Take these vectors as input to the array and see result
result <- array(c(vector1,vector2),dim = c(3,3,2))
print(result)

for(i in seq_len(dim(result)[3])){
  print(i) # there are just 2 layers in this array (ie. one matrix on top of another)
  print(dim(result)) # each layer has dimensions of 3, 3, 2
  print(dim(result)[1]) # good, 4 gives an error. dim(result)[1] is number rows, dim(result)[2] is number columns, dim(result)[3] is the layer number.
}

########## END OF PLAYING WITH ARRAYS ###################################

# And now, Will's plants array:
# layer1 is the terrain matrix (my.world). Yes? No?
# layer2 will be a matrix of plants (first round with random generation based upon user input of number of each plant). Yes? No?
# layer3 and onward will be the new.plants.matrices of plants that we generate through each plant.timestep. 
# Hence, plant.timestep is going to have to have a lot more additions to it!!!!!

## Is this plants array for just getting the initial plant layer? Or is this to be used repeatedly??
## Well, self, plants is called in the for loop in Will's skeleton. Thus, let's initialize plants array prior to the for loop. 
## Why does Will have 3, where I added layer?

######## PLANTS ARRAY #############
# create your initial plant array based upon user input:
# Given the species you gave me, I'm talking to you the user, give me the approx. proportions of each plant you want in the initial matrix (plants will be deducted given the presense of water in the terrain cell)

######### temporary testing input ##########
init.plant.amt <- c(0.3, 0.4, 0.2, 0.1)
data <- rnorm(10, 1, 20)
my.world <- matrix(data =data, nrow = 5, ncol = 5)
######### end temporary testing input ###############

init.plant.matrix <- function(init.plant.amt, my.world){
  if(length(init.plant.amt) != length(names)){
    stop("You have to enter a number for EACH species")
  }
  if(sum(init.plant.amt) != 1){
    stop("Your starting portions must add up to 1")
  }
  size <- nrow(my.world)
  numcells <- size^2
  plantrow <- c(1:size)
  plantcol <- c(1:size)
  # create empty matrix of size == my.world
  plant1.matrix <- matrix(data = NA, nrow = nrow(my.world), ncol = ncol(my.world))
  # create a list of the plants for the number of cells in the matrix, with correspoinding probabilities
  sp <- sample(names, numcells, prob = init.plant.amt, replace = TRUE)
  # now fill the matrix. My logic is that the sp above is random, so I can enter that into the matrix systematically and still have a random matrix.
  count <- 1
    for(i in 1:size){
      for(j in 1:size){
        plant1.matrix[i,j] <- sp[[count]]
        count <- count+1
      }
    }
  return(plant1.matrix)
}

plant.matrix <- init.plant.matrix(init.plant.amt, my.world)
plant.matrix

##### Have to come back to this. Will, you didn't give enough info in notes for my brain to follow your logic.
### I understand that I have to put each "generation" of plants into an array, but your skeleton below....
#### it doesn't connect with me yet. I'll come back to this.

# layer will be the iteration of the array: plants[,,layer]
plants <- array("", dim=c(dim(my.world),timesteps+1))
# for loop is just for going through layers....
for(i in seq_len(dim(plants)[layer]))
  plants[,,i][is.na(my.world)] <- NA

###### END OF PLANTS ARRAY ##########

######## REPRODUCTION ######################
##### like survive function.....but how/where/what...argh! I need to know when/how we are going to tell the functions for which bloody plant species?!?!?! Below, x refers to which plant species. Grrrrrr. Do we loop through each plant species? If so, then there's a bias for the plant that's first in order of looping.
reproduce <- function(row, col, plants, info){
  possible.locations <- as.matrix(expand.grid(row+c(-1,0,1), col+c(-1,0,1)))
  for(i in 1:nrows(possible.locations)){
    r <- possible.locations[i,1]
    c <- possible.locations[i,2]
    # make sure within matrix
    if(r > 0 <= size & c > 0 <= size ){
      if(my.matrix[r,c] == ''){
        repro <- info$repro 
        if(runif(1) <= repro[[x]]){   #just probability form the info$repro at location i
          plant <- names(repro[x])
          plants[r,c] <- plant
        }else{
          plants[r,c] <- ""
        }
      }
    }
  }
  return()
} 

      #...now filter out which ones are not water-logged and reproduce there...
    #...being careful to check you do have somewhere to reproduce to!...
fight <- function(something){
  
  whatisthis <- sample(names, 1, prob=comp.mat[row,column])
  
}


#     
#  }
  
#  return(plants)
#}


#####  let's see how Will is reproducing....you know what I mean ########
row <- 3
col <- 2

possible.locations <- as.matrix(expand.grid(row+c(-1,0,1), col+c(-1,0,1)))
possible.locations
#########  OK, I get it. Now, I figure how you probably created your terrain matrix. #######

##Will's skeleton of plant.timestep used terrain is his input matrix. My terrain is: my.world
## But where do we define x, which is the plant species that we are looking at???
## We call the survive function here, but no where do we define for which darned species!!

# removing:for(i in 1:nrow(my.world)){ and for(j in 1:ncol(my.world)){
#######  PLANT.TIMESTEP FUNCTION ############
plant.timestep <- function(my.world, info){
  new.plants.matrix <- matrix(data = NA, nrow= nrow(my.world), ncol=ncol(my.world))
  # Now we go through every cell in my.world to deterimine if we can put a plant in that cell location
  # of our new matrix: new.plants.matrix. Where size <- nrow(my.world)
  cell.locs <- as.matrix(expand.grid(1:size, 1:size))
  for(i in 1:nrow(cell.locs)){
    r <- cell.locs[i,1]
    c <- cell.locs[i,2]
    newplant <- survive(my.world[r,c], info)
    new.plants.matrix[r,c] <- newplant
  }
  return(new.plants.matrix)
}

##### END OF PLANT.TIMESTEP FUNCTION ###########



