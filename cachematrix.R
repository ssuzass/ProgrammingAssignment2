## MakeCacheMatrix is a function that creates a special "matrix" object that can cache 
## its inverse. The cachesolve computes the inverse of the "matrix" 
## created by makeCacheMatrix and store the value in the object m in the 
## makeCacheMatrix.  In case of the  inverse is already computed, "cacheSolve" 
## function gets the inverse from the cache. It is important to notice that the 
## function cacheSolve assumes that the input is a invertible matrix.
##                                                 -----------------


#----------------------------- makeCacheMatrix------------------------------
##Function “makeCacheMatrix” creates a special “matrix” object x that can cache 
##its inverse in the object m. The function setsolve store the input created by 
## cacheSolve function in m and the function getsolve returns it.
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

#---------------------cacheSolve
## This function first check if the inverse of special “matrix” returned by
## MakeCacheMatrix has already been computed. If not, it computes the inverse 
## of the input and x$setsolve(m) stores it in the object m in makeCacheMatrix. 
##If it has been computed, it retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
