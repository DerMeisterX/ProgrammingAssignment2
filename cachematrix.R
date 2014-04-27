makeCacheMatrix <- function(x = matrix()) {
#The first assignment function
  m  <- NULL #Initialize to NULL
  init  <- function(y){
    x <<- y
    m <<- NULL 
  }
  get  <- function() x
  #Now to set and get inverses 
  setinverse  <- function(inverse) m  <<- inverse
  getinverse  <- function() m
  #Build the data list
  list(set= init, get = get, setinverse = setinverse, getinverse = getinverse)
  

}


## Function to calculate the inverse of the special matrix;
# then retriever cache solve 
## 

cacheSolve <- function(x, ...) {
  m  <- x$getinverse()
  if ( !is.null(m) ){
    message("caching the data")
    return(m)
  }
  mydata  <- x$get()
  m  <- solve(mydata, ...)
  x$setinverse(m)
  m
}
