## functions do
rm(list=ls())

## Below are two functions "makeCacheMatrix" and "cacheSolve"that will be used to create a special object that stores a matrix and caches its inverse. This assignment is called"Caching the Inverse of a Matrix"
##The first function, makeCacheMatrix() creates a special "matrix", This function creates a special "matrix" object that can cache its inverse.
## The 2nd function, cacheSolvecomputes the inverse of the special "matrix" returned by makeCacheMatrix above.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  ##set the value of the matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  ##get the value of the matrix
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## cacheSolve() function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then cacheSolve retrieves the inverse from the cache.
##The following function calculates the inverse of the special "vector" created with the above function. However, it first checks to see if the inverse has already been calculated. If so, it gets the inverse from the cache and skips the computation. Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setinverse function.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
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

## The following lines are there for testing the above code
## mc1 creates a matrix
mc1<-makeCacheMatrix(matrix(c(1,2,3,4),nrow = 2))
#Executing mc1$get function will display the matrix that was created  
#[,1] [,2]
#[1,]    1    3
#[2,]    2    4
mc1$get()


mc1$getinverse()
#creating data to obtain the matrix value from mc1get() function.
data <- mc1$get()
data
#  [,1] [,2]
#[1,]    1    3
#[2,]    2    4
##1st run of cacheSolve function with input variable as mc1 provides the inverse of the matrix
cacheSolve(mc1)
##  [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5

##2nd run of the CacheSolve function now will return data from cache
cacheSolve(mc1)
##getting cached data
##[,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5