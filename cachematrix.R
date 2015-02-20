## The 2 functions 'makeCacheMatrix' and 'cacheSolve' are used in combination to calculate the inverse of a given matrix and
## store the value in cache. If the inverse of the previous matrix is to be calculated again, the value is fetched from the ## cache, else the new inverse is computed and cached. 


## 'makeCacheMatrix' function takes a matrix and returns a vector list of functions to be used by the next function to 
## calculate the inverse and also to cache the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL   ## placeholder for inverse of matrix
  
  ## function to set x and reset i in the parent environment using lexical scoping
  
  set <- function(y) {
    x <<- y 
    i <<- NULL  
  }
 
  ## define the functions needed for the next 'cacheSolve' function
  
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  
  ## Return the list of the functions defined above
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



## 'cacheSolve' function to compute inverse of the matrix using the matrix returned from 'makeCacheMatrix' after checking 
## cached data. 

cacheSolve <- function(x, ...) {
  ## getting and checking cached data 
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  ## compute, cache and return inverse of matrix
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}











