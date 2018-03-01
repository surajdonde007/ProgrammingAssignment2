## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##The following two functions are used to calculate the inverse of a matrix, if the matrix has been cached before, the inverse of a matrix will be returned right now rather than compute it repeatedly
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)

}


## Write a short comment describing this function
##The following function cacheSolve calculates the inverse of the special "matrix" created with the above function it first checks to see if the inverse of the matrix has already been calculated. If so, it gets the inverse from the cache and skips the computation. Otherwise, it calculates the inverse of the data and sets the inverse matrix in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
        ## Return a matrix that is the inverse of 'x'
}
