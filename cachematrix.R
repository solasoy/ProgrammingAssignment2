## The functions below demonstrate the use of scoping functions to calculate
## the inverse of a matrix


## "makeCacheMatrix.R" returns a list of function calls for managing the cache that
## stores a matrix inverse

makeCacheMatrix <- function(x = matrix()) {
  
  m <- solve(matrix())
  
  set <- function(y) {
    f <- function(v) class(try(solve(v),silent=T))=="matrix"
    if (f(y)==TRUE) {
      x <<- y
    } 
    else {
      stop("The input matrix is not invertible!")
    }
    
    m <<- solve(matrix())
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}


## "cacheSolve.R" returns the inverse of a matrix if it already exists in the cache and
## determines the inverse of the matrix if it does not exist in the cache

cacheSolve <- function(x) {
  
  if(!is.na(x$getinv())) {
    message("getting cached data...")
    m <- x$getinv()
    return(m)
  }
  
  data <- x$get()
  inv <- solve(data)
  x$setinv(inv)
  inv
}
