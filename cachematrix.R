## Following pair of functions caches the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
 inv_m <- NULL
  set <- function(y) {
    x <<- y
    inv_m <<- NULL
  }
  get <- function() x
  set_inverse <- function(solve_) inv_m <<- solve_
  get_inverse <- function() inv_m
  list(set = set, get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.



cacheSolve <- function(x, ...) {
  inv_m <- x$get_inverse()
  if(!is.null(inv_m)) {
    message("getting cached data")
    return(inv_m)
  }
  
  data <- x$get()
  inv_m <- solve(data, ...)
  x$set_inverse(inv_m)
  inv_m
}