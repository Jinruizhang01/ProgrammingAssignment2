## R programming week 3 assignment: Caching the Inverse of a Matrix

## makeCacheMatrix: This function creates a special "matrix" object that can 
## cache its inverse.
## cacheSolve:  computes the inverse of the special "matrix" returned by
## makeCacheMatrix above

#set the value of the vector
#get the value of the vector
#set the value of the solve(x)
#get the value of the solve(x)

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

## cacheSolve:  computes the inverse of the special "matrix" returned by
## makeCacheMatrix above

cacheSolve <- function(makeCacheMatrix, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- makeCacheMatrix$getsolve()
  if(!is.null(m)) {
    message("getting cached solve")
    return(m)
  }
  data <- makeCacheMatrix$get()
  m <- solve(data, ...)
  makeCacheMatrix$setsolve(m)
  m
}



