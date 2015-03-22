## The function makeCacheMatrix creates a set
## of functions that cache and strore the inverse
## of an invertible matrix provided in input

makeCacheMatrix <- function(x = matrix()) {
  ## Initialize the local variable m to null
  m <- NULL
  ## create a function called set to set the m to null and 
  ##  set the cached x value to the passed in matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  ## function to return the matrix that is passed
  get <- function() x
  ## function to set the inverse to the temporary variable
  setinv <- function(inv) m <<- inv
  ## function to obtain the stored inverse from cache
  getinv <- function() m
  ## return the 4 functions as output of the code
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv) 
}


## The function cacheSolve checks if the inverse of a matrix  
## exists and returns it, else computes inverse and returns
## it used the inbuilt function solve to compute the inverse

cacheSolve <- function(x, ...) {
  ## calls the getinv function defined in makeCacheMatrix
  ## to obtain any previously computed inverse
  inv <- x$getinv()   
  ## If the inverse obtained is not null, then return the 
  ## inverse with the text "getting cached data"
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  ## if inverse is null, then obtain the numeric matrix that
  ## needs to be evaluated and compute its inverse
  data <- x$get()
  inv <- solve(data, ...)
  ## store the inverse in the cache for future use
  x$setinv(inv)
  ## return the inverse
  inv
}
