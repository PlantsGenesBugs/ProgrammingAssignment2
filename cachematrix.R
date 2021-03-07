## The functions below calculate the inverse of a matrix and caches the output for future use
## Therefore the calculation only has to be done once and saves computing time

## The first function creates a special "vector" that contains a list of four functions,
## i.e. it creates a box that contains functions to set the value of the input matrix,
## get the value of the input matrix, set the value of the inverted matrix, and get the 
## value of the inverted matrix.

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


## The second function first tests whether there's already a value for the inverted matrix
## in the cache by inspecting the getmatrix_inv() function in the output of the makeCacheMatrix
## function. If this returns a value, the function returns the message "getting cached data" and
## returns the value in the cache. If no value (NULL) is returned, the function will proceed to 
## calculate the inverse of the input matrix and stores it in the cache. 

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
