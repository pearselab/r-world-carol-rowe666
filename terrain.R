# Create an empty matrix of appropriate size:
# Make sure your world.size is SQUARE AND must be (x^2) + 1 to work!
world.size <- function(x){
  size <- (2^x) + 1
  my.world <- matrix(data = NA, nrow = size, ncol = size)
  sizeandmatrix <- list(size = size, my.world = my.world)
  return(sizeandmatrix)
}

## Ugh. Have to return multiple values in a darned list in R
## How do you get the return values you want from a darned function?
worldlyy <- world.size(3)
size <- worldly$size
print(size)
my.world <- worldly$my.world

mat.points <- c(1, worldly$size)

### Add values to the four corners of the intitial box  
# my.world[1,1] <- rnorm(1, 20, 200)
# my.world[1,size] <- rnorm(1, 20, 200)
# my.world[size, size] <- rnorm(1, 20, 200)
# my.world[size,1] <- rnorm(1, 20, 200)

## Initialize a vector with numbers corresponding to box size.
# Will add to this vector with additonal locations for boxes within boxes
get.corners <- function(a,b){
  ul <- "my.world[a, a]"
  ur <- "my.world[a, b]"
  ll <- "my.world[b, a]"
  lr <- "my.world[b, b]"
  corners <- list(ul = ul, ur = ur, ll = ll, lr = lr)
  return(corners)
}

mat.points <- c(1, size)
for (i in 1:(length(mat.points)-1)){
  a <- mat.points[i]
  b <- mat.points[i+1]
  corners <- get.corners(a,b)
  ul <- noquote(corners$ul)
  print(ul)
  ul <- rnorm(1, 20, 200)
  noquote(corners$ur) <- rnorm(1, 20, 200)
  noquote(corners$ll) <- rnorm(1, 20, 200)
  noquote(corners$lr) <- rnorm(1, 20, 200)
  #get.corners(a,b)$ul <- rnorm(1, 20, 200)
  #combos <- as.vector(outer(mat.points, mat.points, paste, sep=","))
  }
print(my.world)

?noquote
#This get the middle value from avg. fo 4 corners (shouldn't this be called th square step then?)
# Contrary to class notes, I am calling this my BOXES function since I use the 4 corners of a box
boxes <- function(mat.points) {
  my.midpoints <- c()
  for (i in 1:(length(mat.points)-1)){
    a <- mat.points[i]
    is.numeric(a)
    b <- mat.points[i+1]
    is.numeric(b)
    ul <- my.world[a, a]
    ur <- my.world[a, b]
    ll <- my.world[b, a]
    lr <- my.world[b, b]
    avg <- (ul + ll + ur + lr)/4
    midpoint <- (mat.points[i] + mat.points[i+1])/2
    my.world[midpoint, midpoint] <- avg
    my.midpoints <- c(my.midpoints, midpoint)
  }
  mat.points <- c(mat.points, my.midpoints)
  mat.points <- sort(mat.points, decreasing = FALSE)
  pointsandmatrix <- list(mat.points = mat.points, my.world = my.world)
  return(pointsandmatrix)
}

## Is this really how one gets the return values from a function????
mat.points <- boxes(mat.points)[[1]]
my.world <- boxes(mat.points)[[1]]
print(mat.points)

### And now, the diamonds. Need a recursive function off the box function above???
boxes <- function(mat.points) {
  my.midpoints <- c()
  for (i in 1:(length(mat.points)-1)){
    a <- mat.points[i]
    is.numeric(a)
    b <- mat.points[i+1]
    is.numeric(b)
    ul <- my.world[a, a]
    ur <- my.world[a, b]
    ll <- my.world[b, a]
    lr <- my.world[b, b]
    avg <- (ul + ll + ur + lr)/4
    midpoint <- (mat.points[i] + mat.points[i+1])/2
    my.world[midpoint, midpoint] <- avg
    my.midpoints <- c(my.midpoints, midpoint)
  }
  mat.points <- c(mat.points, my.midpoints)
  mat.points <- sort(mat.points, decreasing = FALSE)
  pointsandmatrix <- list(mat.points, my.world)
  return(pointsandmatrix)
}









## Going to create and add on to the vector each time, assigning values to new additions to the vector
# so, start with 1,9 next time is: 1,5,9 (new is 9), next is 1,9,5,3,7 (new is 3,7)
# Need to get a mechanism to let it know how many are new....compare vectors
row.vector <- c(1, 5, 9)
column.vector <- c(1, 5, 9)
for (i in row.vector){
  for (j in column.vector){
    if (is.na(world.size[i,j])){
      world.size[i,j] <- rnorm(1, 20, 200)
    } else{
      cat("UH oh, there's a number already there:", i, j, "\n")
    }
  }
}

print(world.size)