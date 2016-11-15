#' plants.R builds matrices, size of my.world from terrain.R, with plants over time
#' initial amount and type of plant is determined by user
#' user defines survival, reproductive, and competetive probabilities
#' user also defines the number of iterations/timesteps to go through
#' 
#' Items needed from the terrain.R: my.world
#'
#'Items user needs to input for plants.R: 7 as follows
#' names  # vector of plants
#' repro  # vector of reproduction probabilities of each plant -order as per names
#' survive  # vector of survival probabilities of each plant - order as per names
#' comp.mat  # competitive matrix - order as per names
#' init.plant.ratio # percentage of initial plant coverage - order as per names, sum = 1
#' pctfill  # percent of available cells to be initially covered with plants
#' timesteps  # how many iterations do you want this to go through
#' 
#' Note on reproduction:
#' plant has potential to reproduce as long as one or more plant is alive
#' plant can reproduce in empty cell only if a parent plant within one cell distance away

######## SETUP.PLANTS FUNCTION ################
# make sure user inputs appropriate reproductive, survival, and competitive probabilities
# make sure number of names matches length of probability vectors/matrix
# if no names input, then they will automaticaly be labeled a, b, c, etc.
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
  if(nrow(comp.mat) != ncol(comp.mat) | nrow(comp.mat) != length(repro)){
    stop("Check the size of your competitive matrix. Must have dimensions reflecting number of plant species.")
  }
  if(any(repro > 1) | any(repro < 0)){
    stop("Probabilities out of range in repro vector")
  }
  if(any(survive > 1) | any(survive < 0)){
    stop("Probabilities out of range in survive vector")
  }
  if(any(comp.mat > 1) | any(comp.mat < 0)){
    stop("Probabilities out of range in competition matrix")
  }
  # Keep names tied to respective reproductive and survival probabilities
  repro <- setNames(repro, names)
  survive <- setNames(survive, names)
  return(list(repro=repro, survive=survive, comp.mat=comp.mat,
              names=names))
}
###### END OF SETUP.PLANTS FUNCTION ############

######## MAXFILL FUNCTION ################
# making a matrix from terrain with cells to be filled with plant species
# all other cells are < 0 or NA, so can be ignored - as far as I can tell 

# maxfill is the maximum number of cells that can be filled with plants
# noplant is a matrix of cell positions that can be filled in any following plant matrices
maxfill.matrix <- function(my.world){
  goplant <- which(my.world > 0, arr.ind = TRUE) # matrix of cells that can have plants in them
  maxfill <- nrow(goplant)  # number of cells that can have plants in them
  return(list(maxfill=maxfill, goplant=goplant))
}
######## END MAXFILL FUNCTION ###################

########## If you ever need the NO-fill cells #########
nofill.matrix <- function(my.world){
  naplant <- which(is.na(my.world), arr.ind = TRUE)
  negplant <- which(my.world < 0, arr.ind = TRUE)
  if(length(naplant > 0) & length(negplant > 0)){
    noplant <- rbind(naplant,negplant)
  }else{
    if(length(naplant) >0){
      noplant <- naplant
    }
    if(length(negplant) > 0 ){
      noplant <- negplant
    }
  }
  return(noplant)
}
######## end: if you ever need the NO-fill cells ######

########### INITIAL PLANT MATRIX ################
# User gets to chooose BOTH the proportions of initial plants (init.plant.amt), as well as 
# total percent of fillable cells to be filled with the initial plants.

init.plant.matrix <- function(init.plant.ratio, pctfill, my.world, maxfill, goplant){
  size <- nrow(my.world)
  if(length(init.plant.ratio) != length(names)){
    stop("You have to enter a percentage as a decimal for EACH species")
  }
  if(sum(init.plant.ratio) != 1){
    stop("Your starting plant ratios/percentages are entered as decimal values and must add up to 1.")
  }
  size <- nrow(my.world)
  numcells <- size^2
  num.initplants <- round(pctfill*maxfill/100)
  # create matrix with data <- '' of size == my.world
  plant1.matrix <- matrix(data = '', nrow = size, ncol = size)
  noplant <- nofill.matrix(my.world)
  # fill in neg/na with na
  for(i in 1:nrow(noplant)){
    r <- noplant[[i,1]]
    c <- noplant[[i,2]]
    plant1.matrix[r,c] <- NA
  } 
  if(pctfill == 100){
    blanks <- 0
  }else{
    blanks <- (maxfill - num.initplants)
  }
  if(blanks >= 1){
    # grab random cells to fill in with ''
    blankcells <- sample(nrow(goplant), blanks, replace=FALSE) # gets row index to sample from
    # fill those random cells with ""
    for(i in 1:blanks){
      rrow <- blankcells[i]
      r <- goplant[rrow,1]
      c <- goplant[rrow,2]
      plant1.matrix[r,c] <- ""
    }
    # remove blank cells from goplant matrix
    goplant <- goplant[c(-blankcells),]
  }
  numsample <- (maxfill - blanks)
  sp <- sample(names, numsample, prob = init.plant.ratio, replace = TRUE)
  # Since the sp list is sampled, it is random. Thus, can fill in cells systematically
  for(i in 1:length(sp)){
    #rrow <- goplant[i] # working off index
    r <- goplant[[i,1]]
    c <- goplant[[i,2]]
    plnt <- sp[[i]]
    plant1.matrix[r,c] <- plnt
  }
return(plant1.matrix) 
}
########### END OF INITIAL PLANT MATRIX ##################################

############ ROULETTE FUNCTION #############################
roulette <- function(infoprob, who){
  if(who != ""){
    if(runif(1) <= infoprob[[who]]){
      survivor <- names(infoprob[who])
    }else{
      survivor <- ""
    }
  }else{
    survivor <- ""
  }
  return(survivor)
}

######## REPRODUCTION ######################
# if one or more parent plant exists within ONE cell's distance of empty cell,
# then parent plant can potentially reproduce in empty cell if it out-competes other
# parent plants meeting the same criteria

reproduce <- function(plants, size, info, comp.mat, names, before, step){
  # This gets a matrix of cell locations that can potentially be reproduced into
  blank.matrix <- which(plants[,,before] == "", arr.ind = TRUE)
  for(i in 1:nrow(blank.matrix)){
    r <- blank.matrix[i,1]
    c <- blank.matrix[i,2]
    # make sure within matrix
### 1st part #########
    if((r > 0 & r <= (size-1)) & (c > 0 & c <= (size-1))){
      rmin <- (r-1)
      rmax <- (r+1)
      cmin <- (c-1)
      cmax <- (c+1)
      parentspace <- plants[rmin:rmax, cmin:cmax, before]
    } else{
      if(r == 1){
        rmin <- 1
        rmax <- 2
      }
      if(r==size){
        rmin <- (r-1)
        rmax <- r
      }
      if(c==1){
        cmin <- 1
        cmax <- 2
      }
      if(c==size){
        cmin <- (r-1)
        cmax <- r
      }
      parentspace <- plants[rmin:rmax, cmin:cmax, before]
    }
### 2nd part ####
    #see which potential parents exist around the empty cell
    potparents <- parentspace[which(!is.na(parentspace) & parentspace != "", arr.ind = TRUE)]
    cat("potential parents are:", "\n")
    print(potparents)
    if(length(potparents == 1)){
      newplant <- roulette(repro, potparents[1])
      plants[r,c,step] <- newplant
      #offspring <- newplant
    # given several potential parents, see who successfully reproduces and add to vector of offspring  
    }else if(length(potparents == 2)){
      newplant <- competition(potparents, comp.mat, names)
      plants[r,c,step] <- newplant
    }else if (length(potparents) > 2){
      ## potparents > 2
      newplant <- competition(potparents, comp.mat, names)
      cat("Here's your BULLY:", newplant, "\n")
      plants[r,c,step] <- newplant
    }
  }
  return(plants)
}

###########  COMPETITION FUNCTION ###########################################
# from list of all successfully reproduced plants, need to see which plant
# combos gives the highest prob. of success. Highest prob. from comp.mat
# is the bully: kills it's competition and survives for at least another round

# comp.mat read as: probability of column beating the given row

competition <- function(potparents, comp.mat, names){
  #require(mgcv) # this is so I can simply use uniquecombs to get unique rows in a matrix
  # get all posible combinations within offspring list
  if(length(potparents == 2)){
    alluniquoffspring <- matrix(c(potparents[1], potparents[2],potparents[1], potparents[2]), nrow = 2, ncol = 2)
  }else if (length(potparents) > 2){
    offspring.mat <- combn(potparents, 2)
    # now just get unique combinations
    uniq.offspring <- uniquecombs(offspring.mat)
    # now get the reverse order of these
    revoffspring <- apply(uniq.offspring, 2, rev)
    # combine and get unique again for final matrix of combos to go through in comp matrix
    alluniquoffspring <- uniquecombs(rbind(uniq.offspring, revoffspring))
  }
  # get probabilities from comp.matrix for all possible combinations
  compprob <- numeric(0)
  for(i in 1:length(alluniquoffspring)){
    r <- alluniquoffspring[i,1]
    cat("r is:", r, "\n")
    rr <- which(names==r)
    cat("rr is:", rr, "\n")
    c <- alluniquoffspring[i,2]
    cc <- which(names==c)
    cat("cc is:", cc, "\n")
    # add to vector then do cbind
    prob <- comp.mat[rr,cc]
    compprob <- c(compprob, prob)
    cat(compprob, "\n")
  }
  biggestbully <- which(compprob == max(compprob))
  print(biggestbully)
  if(length(biggestbully == 1)){
    newplant <- names[biggestbully]
  }
  if(length(biggestbully > 1)){
    loto <- sample(biggestbully, 1)
    newplant <- names[lotto]
  }
  return(newplant)
}

######### END COMPETITION FUNCTION #########################




########## INPUT NEEDED #################################
names <- list('Betula', 'Popoulus', 'Pteridium', 'Pseudotsuga') # names of your plant species
repro <- c(0.6, 0.3, 0.4, 0.9) # reproduction probabilities - FOLLOW index order of plants!
survive <- c(0.4, 0.4, 0.6, 0.8) # survival probabilities - FOLLOW index order of plants!
comps <- runif(n=length(repro), min=0, max=1)
comps <- runif(n=length(repro)^2, min = 0, max = 1)
comp.mat <- matrix(comps, nrow = length(repro), ncol = length(repro)) # competition matrix - FOLLOW index order of plants!
data <- rnorm(289, 20, 30)
my.world <- matrix(data =data, nrow = 17, ncol = 17) # from terrain.R
pctfill <- 80 # what percent of available cells would you like to fill
init.plant.ratio <- c(0.4, 0.3, 0.2, 0.1)  # must sum to 1 - ratios for fillining initial plant matrix
timesteps <- 5 # how many iterations to go through
########### END OF INPUT NEEDED #######################

########## HERE WE GO ########################
#### inititalize - only done on first step ####
size <- nrow(my.world)
info <- setup.plants(repro, survive, comp.mat, names)
names <- info$names
repro <- info$repro
survive <- info$survive
comp.mat <- info$comp.mat
# get the cells that can be filled with plants
outmax <- maxfill.matrix(my.world)
maxfill <- outmax$maxfill
goplant <- outmax$goplant
# set up initial matrix of plants - user input required
plant1.matrix <- init.plant.matrix(init.plant.ratio, pctfill, my.world, maxfill, goplant)
cat("Here's the initial plant matrix:", "\n")
print(plant1.matrix)
# set up plants array: layer1 = plant1.matrix that was initialized above
# since layer one is already accounted for, add 1 to the timesteps
plants <- array("", dim=c(dim(my.world),(timesteps+1)))
plants[,,1] <- plant1.matrix[,]
for(step in 2:(timesteps+1)){
  before <- step -1
  plants[,,step] <- plants[,,before]
  print(plants[,,step])
  for(i in 1:nrow(goplant)){
    r <- goplant[i,1]
    c <- goplant[i,2]
    newplant <- roulette(survive, plants[r,c,step])
    plants[r,c,step] <- newplant
  }
  plants <- reproduce(plants, size, info, comp.mat, names, before, step)
  cat("after reproduction:", "\n")
  print(plants[,,step])
}

######### FUNCTION REMINERS ###################################
# setup.plants <- function(repro, survive, comp.mat, names=NULL)
# # return(list(repro=repro, survive=survive, comp.mat=comp.mat,names=names))
# maxfill.matrix <- function(my.world)
# #  return(list(maxfill=maxfill, goplant=goplant))
# init.plant.matrix <- function(init.plant.ratio, pctfill, my.world, maxfill, goplant)
# #  return(plant1.matrix) 
# roulette <- function(infoprob, who)
# #  return(survivor)
# reproduce <- function(newplants.matrix, size, info, comp.mat, names)
#   competition <- function(offspring, comp.mat, names)
#     #  return(newplant)
# #  return(newplants.matrix) 
######### END FUNCTION REMINDERS #############################
