## Matrix inversion (using cache). 


## Creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) 
  {
    inv<-NULL
    set <- function(y) 
      {
        x <<- y
        inv <<- NULL
      }
    get <- function() x
    setinv <- function(solve) inv <<- solve
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
  }


## Computes the inverse of the special "matrix" returned by makeCacheMatrix 
## or  should retrieve the inverse from the cache 
## (if the inverse has already been calculated).
cacheSolve <- function(x, ...) 
  {
    inv <- x$getinv()
    if(!is.null(inv)) 
      {
        message("Getting cached data!")
        return(inv)
      }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
  }
