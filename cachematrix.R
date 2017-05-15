## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  #Set Matrix inverse is NULL
  matInv <- NULL
  # function to set the matrix - 'x' to a new matrix - 'y' 
  # and resets the matrixInverse - 'matInv' to NULL
  set <- function(y) {
    x <<- y
    matInv <<- NULL
  }
  # function to return the Matrix - 'x'
  get <- function() x
  # function to set the matrix Inverse 'matrixInv' to 'matInv'
  setInverse <- function(matrixInv) matInv <<- matrixInv
  
  # function to return the Matrix Inverse - 'matInv'
  getInverse <- function() matInv
  
  # returns the 'special vector' containing all of the functions defined above
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  # get the Matrix Inverse using getInverse() function on special vector passed x
  matInv <- x$getInverse()
  # if 'matInv' is not NULL means cache has value
  if(!is.null(matInv)) {
    message("getting cached data")
    return(matInv)
  }
  # otherwise get the data
  data <- x$get()
  # calculate the Matrix Inverse
  matInv <- solve(data, ...)
  # set Matrix Inverse to cache
  x$setInverse(matInv)
  matInv
}
