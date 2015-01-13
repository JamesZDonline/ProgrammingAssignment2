## These functions create a new matrix object that
## stores a matrix and caches its inverse

## This function, called makeCacheMatrix, contains a 
## set of functions that get and set the value of the
## matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  
  list(set=set, get=get,
       setinv = setinv,
       getinv = getinv)

}


## This function calculates the inverse of the cacheMatrix
## Created using the above function.  If the inverse has
## already been calculated, it uses the inverse from the cache
## otherwise it calculates it and sets the cache invrse to
## this calculated variable.

#Note it also prints out that it is getting the cached data just so
#you are aware that is what it is doing.

cacheSolve <- function(x, ...) {
  inv<- x$getinv()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  matrix <- x$get()
  inv <- solve(matrix)
  x$setinv(inv)
  inv
}
