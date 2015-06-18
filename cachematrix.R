## Coursera R programming - Week 3
## Programming assigment 2
## Function for caching the inverse of matrix
## By: VP
## 17/06/2015

##---------------------------------------
## This function creates a special "matrix" object that can cache its inverse.
## Output: a list containing a function to:
##         * set the value of the matrix
##         * get the value of the matrix
##         * set the value of the inverse
##         * get the value of th inverse
## --------------------------------------

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) {
    m <<- solve
  }
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

##------------------------------------------
## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix. 
## If the inverse has already been calculated (and the matrix has not changed),
##  then the cachesolve retrieves the inverse from the cache.
##
## This function assumes that the matrix is always invertible.
##------------------------------------------

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data.")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m  
}

##----------------------------------------------------
## Sample run:
## > x = rbind(c(1, 0, 4), c(1, 3, 4), c(4, 1, 0))
## > m = makeCacheMatrix(x)
## > m$get()
##       [,1] [,2] [,3]
## [1,]    1    0    4
## [2,]    1    3    4
## [3,]    4    1    0

## No cache in the first run
## > cacheSolve(m)
##         [,1]        [,2]      [,3]
## [1,]  0.08333333 -0.08333333  0.2500
## [2,] -0.33333333  0.33333333  0.0000
## [3,]  0.22916667  0.02083333 -0.0625

## Retrieving from the cache in the second run
## > cacheSolve(m)
## getting cached data.
##         [,1]        [,2]      [,3]
## [1,]  0.08333333 -0.08333333  0.2500
## [2,] -0.33333333  0.33333333  0.0000
## [3,]  0.22916667  0.02083333 -0.0625
