makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # This will hold the inverse of the matrix
  
  # Function to set the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL  # Reset the inverse when the matrix is changed
  }
  
  # Function to get the matrix
  get <- function() x
  
  # Function to set the inverse
  setinverse <- function(inverse) inv <<- inverse
  
  # Function to get the inverse
  getinverse <- function() inv
  
  # Return a list of the above functions
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}
cacheSolve <- function(x) {
  inv <- x$getinverse()
  
  # If inverse is already cached, return it
  if (!is.null(inv)) {
    message("Getting cached inverse")
    return(inv)
  }
  
  # If not cached, calculate the inverse
  mat <- x$get()
  inv <- solve(mat)  # solve() computes the inverse
  x$setinverse(inv)       # Store it in the cache
  inv
}


