## Caching the inverse of a matrix
## Below are two functions that are used to create a special object that stores a numeric matrix and cache's its inverse

## makeCacheMatrix is used to create a special "matrix" that contains a list of functions get and set value of matrix
## inverse, respectively

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {        # set the value of the matrix
    x <<- y
    inv <<- NULL
  }
  get <- function() x         # get the value of the matrix
  setinverse <- function(inverse) inv <<- inverse       # set the value of the inverse matrix
  getinverse <- function() inv                          # get the value of the inverse matrix
  list(set = set, get = get,
       setinverse = setinverse, getinverse = getinverse)
}



## cacheSolve calculates the inverse of the special "matrix" created with the above function

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {             # checks to see if the inverse has already been calculated
    print("getting cached data")
    return(inv)                   # gets the inverse from the cache if it exists and skips computation
  }
  data <- x$get()
  inv <- solve(data, ...)         # calculates the inverse of the matrix
  x$setinverse(inv)               # sets the value of the inverse in the cache
  inv                             # return a matrix that is the inverse of 'x'
}
