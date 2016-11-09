#’ Make an elevational grid with optional lakes and rivers
#’
#’ User needs to add values for: lakes, deviation, x for formulating size of matrix, and initial deviation for first four points in matrix



# Create an empty matrix of appropriate size:
# Make sure your world.size is SQUARE AND must be (2^x) + 1 to work!
world.size <- function(x){
  size <- (2^x) + 1
  my.world <- matrix(data = NA, nrow = size, ncol = size)
  sizeandmatrix <- list(size = size, my.world = my.world)
  return(sizeandmatrix)
}

# Get variation around means. Enter how big you want initial deviation.
# function makes a vector from deviation to zero for the number of iterations needed: (size+1)
stdev <- function(deviation, x){
  stdev <- seq(deviation,0, length.out = x+1)
  return(stdev)
}

# This uses the matrix.points vector which originally contains the dimensions of your box.
# This function takes the midpoints bewteen each consecutive pairs of numbers in the matrix.points vector.
# The mid points are then entered into a new matrix: mid.points.
get.midpoints <- function(matrix.points){
  mid.points <- numeric(0)
  for (i in 1:(length(matrix.points)-1)){
    midpoint <- (matrix.points[i] + matrix.points[i+1])/2
    mid.points <- append(mid.points, midpoint)
  }
  return(mid.points)
}

# This boxes function gets values at the four corners of the box.
# The avg of these corners is given to the center cell.
# This works off the midpoints. All combinations of the midpoints gives you all the needed cells that need to be filled in. Work off these midpoint locations to get locations of the corners of the box.
# temp.matrix is a temporary vector (should have named better) containing, in numeric order, all the existing points in matrix.points plus the new mid.points. Thus you can get the values of numbers above and below the value of interest.
boxes <- function(temp.matrix, matrix.points, deviant) {
  for (i in 1:(length(mid.points))){
    for(j in 1:length(mid.points)){
      # x and y are the values of the elements at the i and j index positions
      x <- mid.points[i]
      y <- mid.points[j]
      # xx gets the index number of the value of x in temp.matrix
      xx <- which(temp.matrix %in% x)
      # xxplus and xxminus gets the values one above and one below the the x value
      xxplus <- temp.matrix[xx+1]
      xxminus <- temp.matrix[xx-1]
      # for yy, yyplus, and yyminus, see coresponding x above
      yy <- which(temp.matrix %in% y)
      yyplus <- temp.matrix[yy+1]
      yyminus <- temp.matrix[yy-1]
      # use the values above to get the corners of the box
      corner1 <- my.world[xxminus, yyminus]
      corner2 <- my.world[xxplus, yyminus]
      corner3 <- my.world[xxminus, yyplus]
      corner4 <- my.world[xxplus, yyplus]
      # now average the corners and put that value into the center cell
      avg <- (corner1 + corner2 + corner3 + corner4)/4
      avg <- avg + deviant
      my.world[x,y] <- avg
    }
  }
  return(my.world)
}


# This diamonds function takes (if exists) the box cells above, below, left, and right of the cell to be filled.
# Like the boxes function, we work off the location of the cells to be filled. These cell locations are obtained
# by getting all combinations of the mid.points relative to the matrix.points. 
diamonds <- function(matrix.points, mid.points, temp.matrix, deviant) {
  for (i in 1: length(mid.points)){
    for (j in 1:length(matrix.points)){
      a <- mid.points[i]
      b <- matrix.points[j]
      aa <- which(temp.matrix %in% a)
      aaplus <- temp.matrix[aa+1]
      aaminus <- temp.matrix[aa-1]
      bb <- which(temp.matrix %in% b)
      bbplus <- temp.matrix[bb+1]
      bbminus <- temp.matrix[bb-1]
      first.numbers <- numeric(0)
      second.numbers <- numeric(0)
      # if statemens are just checking that the cell actuall exists; not at edge of box.
      # if cell exists, then get the value from it and put into vector.
      if (a > 1){
        first.top <- my.world[aaminus, b]
        second.left <- my.world[b, aaminus]
        first.numbers <- append(first.numbers, first.top)
        second.numbers <- append(second.numbers, second.left)
      }
      if (a < size){
        first.bottom <- my.world[aaplus, b]
        second.right <- my.world[b, aaplus]
        first.numbers <- append(first.numbers, first.bottom)
        second.numbers <- append(second.numbers, second.right)
      }
      if (b > 1){
        first.left <- my.world[a, bbminus]
        second.top <- my.world[bbminus, a]
        first.numbers <- append(first.numbers, first.left)
        second.numbers <- append(second.numbers, second.top)
      }
      if (b < size){
        first.right <- my.world[a, bbplus]
        second.top <- my.world[bbplus, a]
        first.numbers <- append(first.numbers, first.right)
        second.numbers <- append(second.numbers, second.top)
      }
      first.replacement <- mean(first.numbers) + deviant
      second.replacement <- mean(second.numbers) + deviant
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

waterlogged <- function(lakes){
  if(lakes){
    my.world[my.world<0]= "NA"
    print("you have lakes")
  }else {
    print("No lakes")
  }
  return(my.world)
}

### User needs to add values for: lakes, deviation, x for formulating size of matrix, and initial deviation for first four points in matrix
lakes <- TRUE
deviation <- 50
x <- 3
initdeviation <- 100
print(matrix.points)
stdev <- stdev(deviation, x+1)
worldly <- world.size(x)
size <- worldly$size
matrix.points <- c(1, size)
my.world <- worldly$my.world

### Add values to the four corners of the intitial box  
my.world[1,1] <- rnorm(1, 10, initdeviation)
my.world[1,size] <- rnorm(1, 10, initdeviation)
my.world[size, size] <- rnorm(1, 10, initdeviation)
my.world[size,1] <- rnorm(1, 10, initdeviation)

## Initialize a vector with numbers corresponding to box size.
# Will add to this vector with additonal locations for boxes within boxes

rounds <- 1
difference <- (matrix.points[2] - 1)
while (difference > 1){
  deviant <- stdev[rounds]
  cat("deviant should be:", deviant, "\n")
  mid.points <- get.midpoints(matrix.points)
  temp.matrix <- sort(c(matrix.points, mid.points), decreasing = FALSE)
  my.world <- boxes(temp.matrix, matrix.points, deviant)
  outofdiamonds <- diamonds(matrix.points, mid.points, temp.matrix, deviant)
  matrix.points <- outofdiamonds$matrix.points
  my.world <- outofdiamonds$my.world
  difference <- (matrix.points[2] - 1)
  rounds <- rounds + 1
}

print(my.world)
print(stdev)
my.world <- waterlogged(lakes)
print(my.world)
