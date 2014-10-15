## Caching the inverse of a matrix
## Below are two functions that are used to create a special object that stores a numeric matrix and cache's its inverse.

## makeCacheMatrix is used to create a special "matrix" that contains a list of functions to
## 1.Set the value of the matrix (set)
## 2.Get the value of the matrix (get)
## 3.Set the value of the inverse matrix (setinverse)
## 4.Get the value of the inverse matrix (getinverse)

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse, getinverse = getinverse)
}



## cacheSolve calculates the inverse of the special "matrix" created with the above function.
## It first checks to see if the inverse has already been calculated. If so, it gets the inverse from the cache 
## and skips the computation. Otherwise, it calculates the inverse of the matrix and sets the value of the inverse 
## in the cache via the setmean function.

cacheSolve <- function(x, ...) {
  inv<-x$getinverse()
  if(!is.null(inv)) {
    print("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv        ## Return a matrix that is the inverse of 'x'
}
