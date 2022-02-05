library(matrixcalc) ##The function needs the package matrixcalc to work

##These two functions calculate the inverse of a matrix and caches it, 
##make sure to store your matrix as an object before using the functions

##makeCachematrix takes a matrix (only square), calculates its inverse and caches it
makeCacheMatrix <- function(x = matrix()){
  
  m <- NULL
  set <- function(y){
      x <<- y
      m <<- NULL
  }
  get <- function() x
  ##if the matrix is a square matrix, it calculates its inverse
  setinver <- if (is.square.matrix(x) == TRUE){
          function(solve) m <<- solve
  } ## if the matrix is not a square matrix an error occurs
    else{
          message("the given matrix is not a square matrix")
          stop()
  }
  getinver <- function() m
  list(x, set = set, get = get, setinver = setinver, getinver = getinver)
  
}

## This function returns the inverse stored in the cache or calculates the inverse if it has not been done yet
cacheSolve <- function(x, ...){
  
  m <- x$getinver()
  if(!is.null(m)){
      message("getting cached data")
      return(m)
  }
  data <-x$get()
  m <- if (is.square.matrix(data) == TRUE){
        solve(data, ...)
  } else{
        message("the given matrix is not a square matrix")
  }
  x$setinver(m)
  m
}