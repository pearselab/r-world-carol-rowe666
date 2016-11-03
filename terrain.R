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


mat.points <- c(1, worldly$size)

### Add values to the four corners of the intitial box  
my.world[1,1] <- rnorm(1, 20, 200)
my.world[1,size] <- rnorm(1, 20, 200)
my.world[size, size] <- rnorm(1, 20, 200)
my.world[size,1] <- rnorm(1, 20, 200)

## Initialize a vector with numbers corresponding to box size.
# Will add to this vector with additonal locations for boxes within boxes
mat.points <- c(1, size)


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
outofboxes <- boxes(mat.points)
mat.points <- outofboxes$mat.points
my.world <- outofboxes$my.world
print(my.world)

### And now, the diamonds. Need a recursive function off the box function above???
diamonds <- function(mat.points) {
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
   
   
  }
  
  
  
}

