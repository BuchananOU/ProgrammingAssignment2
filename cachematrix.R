## Programming Assignment 2: based on the special vector functions, these make, cache, and solve special matrices.

## creates a special matrix that can cache its own inverse (if solvable).  uses a list of functions and the matrix "solve" 
## function  NOTE: only square matrices will be solvable by solve()

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <-function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## returns the inverse of a special matrix from makeCacheMatrix. 
## If cached, it uses that value instead of computing it again.  If not, it will compute and cache the value

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
          m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached inverse matrix")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
