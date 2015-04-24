## Put comments here that give an overall description of what your
## functions do

## x is a matrix
## m is a cached invert matrix
## $set() function sets the matrix x
## $get() function returns the matrix x
## $setinv() function stores(does not calculate) to the inverse matrix m
## $getinv() function returns the inverse matrix m
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  ## set a new matrix x and delete the inverse matrix m of x
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  ## return the stored matrix x
  get <- function() x
  ## set the inverse matrix m
  setinv <- function(inv) m <<- inv
  ## return the inverse matrix m
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## the matrix x can be set by the function $set() from makeCacheMatrix()
## cacheSolve() will return the inverse matrix m if it was previously stored by the function $setinv()
## if not, it will calculate the inverse matrix of x using the Solve() function
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  ## if the inverse matrix m already exists, then use that value.
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ## if the inverse matrix m does not exist, calculate m.
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m  
}
