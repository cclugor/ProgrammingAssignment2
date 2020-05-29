#### This is the assignment for week 3 - Programming Assignment 2: Lexical Scoping -----
## The following functions create and cache the inverse of a matrix.

## Function 1- makeCacheMatrix-> creates a matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) { inv <- NULL
set <- function(y){
  x <<- y
  inv <<- NULL
}
get <- function() x
setInverse <- function(solveMatrix) inv <<- solveMatrix
getInverse <- function() inv
list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Function 2- makeCacheMatrix-> computes the inverse of the matrix returned by makeCacheMatrix created above
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("Getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setInverse(inv)
  inv
}

z<-matrix(c(1:4),2)
y<-makeCacheMatrix(z)
cacheSolve(y)

z<-matrix(c(1:4),2)
cacheSolve(y)
