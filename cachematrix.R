## this pair of functions computes the inverse of the matrix
## in case it was not computed earlier and cached;
## otherwise cached result of the previous computation will be returned


## makeCacheMatrix creates a 'wrapper' that can contain a matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  ##inv is used for storing inverse of the matrix x
  inv <- NULL
  set <- function(y) {
      x <<- y
      inv <<- NULL
    }
  get <- function() x
  setinverse <- function(solve)  inv <<- solve
  getinverse <- function() inv
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


##cacheSolve returns inverse of the matrix.
##It takes list created by makeCacheMatrix as argument.
##If inverse is already computed and saved in the list, cacheSolve returns it.
##If no inverse was cached before, cacheSolve computes it and caches.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("Inverse already computed! Using cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
