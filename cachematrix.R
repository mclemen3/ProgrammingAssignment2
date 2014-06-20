## This file contains two functions.
## The purpose of the functions is to return the inverse of a matrix.
## The functions determine if the inverse has already been calculated and cached.
## If so, it returns the cached value. If not, it calculates the value.


## The first function creates several functions that 
##    1. set the value of a matrix
##    2. get the value of the matrix
##    3. calculate the inverse of the matrix
##    4. retrieve the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setinverse <- function(solve) m <<- solve
      getinverse <- function() m
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
      
      
}


## This second function checks to see if the inverse has already been calculated
## If so, it uses the makeCacheMatrix function to retrieve the value
## If not, it uses the makeCacheMatrix function to calculate the value

cacheSolve <- function(x, ...) {
      m <- x$getinverse()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setinverse(m)
      m
}


##    Example code:

##    im    <- makeCacheMatrix()
##    d     <- 4  # or any integer>0
##    im$set(matrix(rnorm(d^2),d,d))
##    cacheSolve(im)
