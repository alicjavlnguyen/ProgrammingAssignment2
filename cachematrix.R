## 'makeCacheMatrix' creates an object which stores a matrix and
## caches its inverse. Use this object as input to 'cacheSolve' (below)
## to retrieve the inverse efficiently.

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y){                    ## stores the matrix
    x <<- y
    s <<- NULL
  }
  get <- function () x
  
  setinv <- function(inverse) { 
    s <<- inverse                        ## stores result of solve(x)
  }
  getinv <- function() s              
  list(set = set, 
       get = get,
       setinv = setinv,
       getinv = getinv)
}


## 'cacheSolve' returns the cached inverse of the matrix (if such exists)
## and skips the calculation. Otherwise, it calculates the inverse and caches
## it using setinv function. Here, the object returned by makeCacheMatrix 
## should be used as 'x'. 

cacheSolve <- function(x, ...) {
  s <- x$getinv()
  if(!is.null(s)){
    message("getting cached data")
    return(s)
  }
 
  data <- x$get()                 ## call the get function to get the matrix
  s <- solve(data, ...)           ## computes inverse
  x$setinv(s)                     ## caches inverse
  s
}
