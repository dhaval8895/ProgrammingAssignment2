## makeCacheMatrix gives the cache vbalue of the matrix and stores this value
## in the form of cache so that it can be accessed later



makeCacheMatrix <- function(x = matrix()) {
  invr <- NULL
  set <- function(y) {
    x <<- y
    invr <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) invr <<- inverse
  getinverse <- function() invr
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


##cachesolve takes the cache value of makeCacheMatrix and gives the inverse 

cachesolve <- function(x, ...) {
  invr <- x$getinverse()
  if(!is.null(invr)) {
    message("getting cached data")
    return(invr)
  }
  data <- x$get()
  invr <- solve(data, ...)
  x$setinverse(invr)
  invr
}
