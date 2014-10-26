## Put comments here that give an overall description of what your
## functions do

## This function returns a list of four functions to be used by the cacheSolve
## function. It also clears the invm cache such that it can be stored to with
## one of those functions. set() sets x as the input matrix and nulls the invm
## cache. get() retrieves the input matrix. setinverse() sets invm as the
## inverse matrix. getinverse() retrieves the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
  invm <- NULL
  set <- function(blah)
    x <<- blah
    invm <<- NULL
  get <- function() x
  setinverse <- function(inverse) invm <<- inverse
  getinverse <- function() invm
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## --------------------------------------------------------

## This function uses the data and functions from makeCacheMatrix() and uses
## them to check if the inverse matrix is already cached. If so, it retrieves
## the data, invm, and returns it. If not, it runs the solve() function, finds
## the inverse matrix, and caches it to invm such that next time cacheSolve()
## is run on the same matrix, invm is not found to be null and is returned.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invm <- x$getinverse()
  if(!is.null(invm)) {
    message("Retrieving Cached Data...")
    return(invm)
  }
  data <- x$get()
  invm <- solve(data, ...)
  x$setinverse(invm)
  invm  
}
