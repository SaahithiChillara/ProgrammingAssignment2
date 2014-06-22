## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix declares a list of four functions
## set - sets the variable of the parent environment
## get - returns the matrix
## setinverse - sets the value of parent variable m 
## getinverse - returns value of parent variable m 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  
  setinverse <- function(inverse) m <<- inverse
  
  getinverse <- function() m 
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
##cacheSolve calculates the inverse of the given matrix
## if it has not been calculated previously. By using 
## getinverse function, it determines if the matrix
## has already been calculated

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m<- x$getinverse()
  
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
}
