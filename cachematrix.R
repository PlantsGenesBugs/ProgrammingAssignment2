## The functions below calculate the inverse of a matrix and caches the output for future use

## The first function creates a special "matrix" that exposes a list of four functions. These functions: sets the value of the
## input matrix, gets the value of the input matrix, sets the value of the inverted matrix, and gets the value of the inverted matrix.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmatrix_inv <- function(matrix_inv) m <<- matrix_inv
  getmatrix_inv <- function() m
  list(set = set, get = get,
       setmatrix_inv = setmatrix_inv,
       getmatrix_inv = getmatrix_inv)
}


## The second function first tests whether there's already a value for the inverted matrix in the cache. If there is, 
## the function returns the message "getting cached data" and returns the value in the cache. If no value (NULL) is found,
## the function will proceed to calculate the inverse of the input matrix, store it in the cache and return it. 

cacheSolve <- function(cache_box, ...) {
  inverse <- cache_box$getmatrix_inv()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- cache_box$get()
  inverse <- solve(data, ...)
  cache_box$setmatrix_inv(inverse)
  inverse
}
