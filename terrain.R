# Create an empty matrix of appropriate size:
# Make sure your world.size is SQUARE AND must be (2^x) + 1 to work!
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
#print(my.world)
#This get the middle value from avg. fo 4 corners (shouldn't this be called th square step then?)
# Contrary to class notes, I am calling this my BOXES function since I use the 4 corners of a box
get.midpoints <- function(matrix.points){
  mid.points <- c()
  for (i in 1:(length(matrix.points)-1)){
    midpoint <- (matrix.points[i] + matrix.points[i+1])/2
    mid.points <- c(mid.points, midpoint)
  }
  return(mid.points)
}


boxes <- function(temp.matrix, matrix.points) {
  for (i in 1:(length(mid.points))){
    for(j in 1:length(mid.points)){
      x <- mid.points[i]
      y <- mid.points[j]
      cat("x is:", x, "\n")
      cat("y is:", y, "\n")
      xx <- which(temp.matrix %in% x)
      xxplus <- temp.matrix[xx+1]
      xxminus <- temp.matrix[xx-1]
      yy <- which(temp.matrix %in% y)
      yyplus <- temp.matrix[yy+1]
      yyminus <- temp.matrix[yy-1]
      corner1 <- my.world[xxminus, yyminus]
      corner2 <- my.world[xxplus, yyminus]
      corner3 <- my.world[xxminus, yyplus]
      corner4 <- my.world[xxplus, yyplus]
      avg <- (corner1 + corner2 + corner3 + corner4)/4
      my.world[x,y] <- avg
      #if x >= y # no, but need to get the reverse orders on these!
      #my.world[y,x] <- avg
    }
  }
  return(my.world)
}


### And now, the diamonds. Need a recursive function off the box function above???
diamonds <- function(matrix.points, mid.points, temp.matrix) {
  for (i in 1: length(mid.points)){
    for (j in 1:length(matrix.points)){
      a <- mid.points[i]
      b <- matrix.points[j]
      aa <- which(temp.matrix %in% a)
      aaplus <- temp.matrix[aa+1]
      aaminus <- temp.matrix[aa-1]
      #cat("aaplus is:", aaplus, "\n")
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
      #cat("first replacement:",first.replacement, "\n")
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


#outofboxes <- boxes(matrix.points, my.world)
rounds <- 1
difference <- (matrix.points[2] - 1)
while (difference > 1){
  mid.points <- get.midpoints(matrix.points)
  cat("midpoints are:", mid.points, "\n")
  temp.matrix <- sort(c(matrix.points, mid.points), decreasing = FALSE)
  my.world <- boxes(temp.matrix)
  outofdiamonds <- diamonds(matrix.points, mid.points, temp.matrix)
  matrix.points <- outofdiamonds$matrix.points
  my.world <- outofdiamonds$my.world
  difference <- (matrix.points[2] - 1)
  rounds <- rounds + 1
}

print(my.world)


