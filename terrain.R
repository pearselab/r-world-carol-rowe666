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
worldly <- world.size(3)
size <- worldly$size
my.world <- worldly$my.world

### Add values to the four corners of the intitial box  
my.world[1,1] <- rnorm(1, 20, 200)
my.world[1,size] <- rnorm(1, 20, 200)
my.world[size, size] <- rnorm(1, 20, 200)
my.world[size,1] <- rnorm(1, 20, 200)

## Initialize a vector with numbers corresponding to box size.
# Will add to this vector with additonal locations for boxes within boxes
matrix.points <- c(1, size)
print(matrix.points)
#This get the middle value from avg. fo 4 corners (shouldn't this be called th square step then?)
# Contrary to class notes, I am calling this my BOXES function since I use the 4 corners of a box
boxes <- function(matrix.points) {
  mid.points <- c()
  for (i in 1:(length(matrix.points)-1)){
    a <- matrix.points[i]
    b <- matrix.points[i+1]
    ul <- my.world[a, a]
    ur <- my.world[a, b]
    ll <- my.world[b, a]
    lr <- my.world[b, b]
    avg <- (ul + ll + ur + lr)/4
    midpoint <- (matrix.points[i] + matrix.points[i+1])/2
    my.world[midpoint, midpoint] <- avg
    mid.points <- c(mid.points, midpoint)
  }
  #mat.points <- c(mat.points, my.midpoints)
  #mat.points <- sort(mat.points, decreasing = FALSE)
  pointsandmatrix <- list(mid.points = mid.points, my.world = my.world)
  return(pointsandmatrix)
}

## Is this really how one gets the return values from a function????
outofboxes <- boxes(matrix.points)
mid.points <- outofboxes$mid.points
my.world <- outofboxes$my.world
#print(my.world)
#cat("matrix points:", matrix.points, "\n")
#cat("mid points:", mid.points, "\n")
temp.matrix <- sort(c(matrix.points, mid.points), decreasing = FALSE)
#cat(temp.matrix)

### And now, the diamonds. Need a recursive function off the box function above???
diamonds <- function(matrix.points, mid.points, temp.matrix) {
  for (i in 1: length(mid.points)){
    for (j in 1:length(matrix.points)){
      a <- mid.points[i]
      b <- matrix.points[j]
      aa <- which(temp.matrix %in% a)
      aaplus <- temp.matrix[aa+1]
      aaminus <- temp.matrix[aa-1]
      cat("aaplus is:", aaplus, "\n")
      bb <- which(temp.matrix %in% b)
      bbplus <- temp.matrix[bb+1]
      bbminus <- temp.matrix[bb-1]
      first.numbers <- c()
      second.numbers <- c()
      if (a > 1){
        first.top <- my.world[aaminus, b]
        second.left <- my.world[b, aaminus]
        first.numbers <- c(first.numbers, first.top)
        second.numbers <- c(second.numbers, second.left)
      }
      if (a < size){
        first.bottom <- my.world[aaplus, b]
        second.right <- my.world[b, aaplus]
        first.numbers <- c(first.numbers, first.bottom)
        second.numbers <- c(second.numbers, second.right)
      }
      if (b > 1){
        first.left <- my.world[a, bbminus]
        second.top <- my.world[bbminus, a]
        first.numbers <- c(first.numbers, first.left)
        second.numbers <- c(second.numbers, second.top)
      }
      if (b < size){
        first.right <- my.world[a, bbplus]
        second.top <- my.world[bbplus, a]
        first.numbers <- c(first.numbers, first.right)
        second.numbers <- c(second.numbers, second.top)
      }
      first.replacement <- mean(first.numbers)
      second.replacement <- mean(second.numbers)
      cat("first replacement:",first.replacement, "\n")
      if (is.na(my.world[a, b])){
        my.world[a, b] <- first.replacement
      }
      if (is.na(my.world[b, a])){
        my.world[b, a] <- second.replacement
      }
    } 
  }
matrix.points <- temp.matrix
diamonds.out <- list(matrix.points = matrix.points, my.world = my.world)
return(diamonds.out)
}

outofdiamonds <- diamonds(matrix.points, mid.points, temp.matrix)
matrix.points <- outofdiamonds$matrix.points
print(matrix.points)
my.world <- outofdiamonds$my.world
print(my.world)

## Now, just loop throgh until matrix is full!
