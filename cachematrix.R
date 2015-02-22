## The 2 functions 'makeCacheMatrix' and 'cacheSolve' are used in combination to calculate the inverse of a matrix and 
## store the value in cache. If the inverse of the previous matrix is to be calculated again, the value is fetched from the
## cache, else the new inverse is computed and cached. 


## 'makeCacheMatrix' function takes a matrix argument, resets the cache value, defines and returns a list of functions
## to be used by the 'cacheSolve' function to calculate the inverse of the matrix and cache it.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL   ## placeholder for inverse of matrix
  
  ## function to set the matrix and reset the cache value of inverse in the parent environment using lexical scoping
  
  set <- function(y) {
    x <<- y 
    i <<- NULL  
  }
 
  ## define the functions needed for the 'cacheSolve' function
  
  get <- function() x                           ## Returns the matrix x
  setinverse <- function(inverse) i <<- inverse ## Sets inverse value to function argument with lexical scoping
  getinverse <- function() i                    ## Returns the inverse value i 
  
  ## return the list of the functions defined above
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



## 'cacheSolve' function to compute inverse of the matrix using the functions returned from 'makeCacheMatrix'
## and the 'solve' function of R after checking the availability of cached data for inverse of matrix. 

cacheSolve <- function(x, ...) {
  
  ## getting and checking cached data for inverse matrix. Return cached value if not NULL
  
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  ## get matrix, compute (using 'solve') , cache and return inverse of matrix if checked cached value above was NULL
  
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}











