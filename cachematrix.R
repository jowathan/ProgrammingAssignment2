#jw 23/05/15 v1.0
#based on pengs PA2
#takes a INVERTABLE (i.e. square) matrix and creates and the functions for 
#generating the inverse of that vector of numerics
#the argument x should take the form of a square matrix
#BEFORE you run this, create a square matrix and assign it to matrix object
#ASSIGN the result of makeCacheMatrix to another object, you'll need it for CacheSolve
makeCacheMatrix <- function(origmat = matrix()) {
  # create set function (which doesn't get used in this context?)
  i <- NULL
  set <- function(y) {
    origmat <<- y
    i <<- NULL
  }
  #make the data available later - by returning the vector that went into the function
  get <- function() origmat
  
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

#cacheSolve
#FUNCTION WHICH CREATES INVERSE MATRIX, AND THEN RETURNS IT AS NEEDED IF IT IS AVAILABLE
#cacheSolve DRAWS ON makeCacheMatrix - assign outputs to the input argument
#Argument takes the name of the result of makeCacheMatrix
cacheSolve <- function(x, ...) {
  #using a different name here, as (the ms in the 2 examples were different)
  i.cache <- x$getinverse()
  # look for a cached inverse matrix in m.cache
  if(!is.null(i.cache)) {
    #let the user know that the cache is being used
    message("getting cached data")
    # return cache and exit function (so no 'else' required)
    return(i.cache)
  }
  #only calculate the inverse if it wasn't already available
  #first retrieve the appropriate data from by calling on the 'get' for the data
  matrix <- x$get()
  #FINALLY! this is where the calculation actually happens!
  i.cache <- solve(matrix)
  # cache result to save hassle next time
  x$setinverse(i.cache)
  # display the result
  i.cache
}

##############################user guide#######################################
# #example showing how to use:
# d <- matrix(1:4, 2, 2)
# #to assign a 2 by 2 matrix containing 1 to 4 to object d
#
# e<-makeCacheMatrix(d)
# #run makeCacheMatrix for your matrix and save outputs to e for reuse
#
# cacheSolve(e)
# #should both run and cache the result if not previously present
###############################################################################

