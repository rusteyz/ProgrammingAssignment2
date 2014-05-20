## makeCacheMatrix.R
##
## Create a cache marix object that can be used to
## repeatably solve the inverse of the marix, but only
## calculates the inverse once of the passed matrix
##
## Usage:
##  M <- matrix(c(1,2,3,4,5,6,7,8,9), nrow=3, ncol=3)
##  cacheMatrix <- makeCacheMatrix(M)
##  cacheSolve(cacheMatrix)
##
##  cacheMatrix$set(M)      # Change the matrix being cached.
##  M <- cacheMatrix$get()  # cache matrix being returned
##
##  cacheMatrix$setInv(solve(data, ...)) # Private function containing cached inverse of 'a' matrix
##  cacheMatrix$getInv()                 # Private function used to get the cached inverse of 'a' matrix

## Create a cacheMatrix object for an matrix.
makeCacheMatrix <- function(a = matrix()) {
  i <- NULL
  set <- function(b) {
    a <<- b
    i <<- NULL
  }
  get <- function() a
  setInv <- function(inverse) i <<- inverse
  getInv <- function() i
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## Following function returns the inverse of an cacheMatrix object passed as a parameter
cacheSolve <- function(a, ...) {
  ## Function returns a matrix 'i' that is the inverse of matrix 'a'
  i <- a$getInv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  ## get the matrix
  data <- a$get()
  ## calculate and assign the inverse of the matrix
  i <- solve(data, ...)
  ## Set the calculated matrix
  a$setInv(i)
  i
}
