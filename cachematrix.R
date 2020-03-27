## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## function takes the matrix and creates a special vector
makeCacheMatrix <- function(mat = matrix()) {
  inv <- NULL
  set <- function(y) {
    mat <<- y
    inv <<- NULL
  }
  get <- function() mat
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function
## function takes the matrix,and checks if inverse is present
## if not calculates inverse else returns inverse
cacheSolve <- function(x, ...) {
  inverse <- x$getinv()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinv(inverse)
  inverse
  ## Return a matrix that is the inverse of 'x'
}

#test <- matrix(1:4, nrow = 2, ncol = 2)
#t <- makeCacheMatrix(test)
#print(test)
#print(class(test))
#cacheSolve(t)
