## These functions create an object which stores a matrix
## and (if available) the inverse of the matrix, so the
## inverse can be reused without calling solve() more than once.

## See example far below

## makeCacheMatrix returns a list of 4 functions
##    set: to set the matrix,
##    get: to get the matrix,
##    setSolve: to set the inverse,
##    getSolve: to get the inverse.
## Internally it holds the matrix in x and the inverse
## in s. s is NULL if the inverse has not been set.
## Setting the matrix (x) automatically resets s to NULL.

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setSolve <- function(matrix) s <<- matrix
  getSolve <- function() s
  list(set = set, get = get,
       setSolve = setSolve,
       getSolve = getSolve)
}

## cacheSolve returns the inverse of a CacheMatrix.
## It uses the cached value if that is available, or
## calculates the inverse with solve() and caches that value.

## The example cachemean function handled the !is.null case first,
## which meant it had two different return functions. By reversing
## the order, I get a shorter, cleaner function - it always returns s,
## which has either come from getSolve() or came from solve() after
## being cached with setSolve()

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  s <- x$getSolve()
  if (is.null(s)) {
    data <- x$get()
    s <- solve(data, ...)
    x$setSolve(s)
  } else {
    message("getting cached data")
  }
  s
}

## Example session
## > source("cachematrix.R")
## > m <- matrix(c(1,2,3,4),nrow=2)
## > m
##      [,1] [,2]
## [1,]    1    3
## [2,]    2    4
## mm <- makeCacheMatrix(m)
## mm$get()
##     [,1] [,2]
## [1,]    1    3
## [2,]    2    4
## > cacheSolve(mm)
##     [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## cacheSolve(mm)
## getting cached data
##     [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## > mm$set(cacheSolve(mm))
## getting cached data
## > mm$get()
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## > mm$set(cacheSolve(mm))
## > mm$get()
##      [,1] [,2]
## [1,]    1    3
## [2,]    2    4
