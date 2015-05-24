## This function tests if the MASS package is installed as it 
## makes use of the ginv() function. If TRUE, the function
## caches the inverse of a matrix, returning a list containing a function
## to store the inverse of a matrix in a cached symbol

makeCacheMatrix <- <- function(mx = matrix()) {
  
  MASStest <<- is.element("MASS", installed.packages()[,1])
  
  if(MASStest == TRUE) {
    m <- NULL
    set <- function(y) {
      mx <<- y
      m <<- NULL
    }
    get <- function() mx
    setinv <- function(ginv) m <<- ginv
    getinv <- function() mx
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
  }
  else {
    print("Please use install.packages() to install the MASS package")
  }
}


## This function tests if the MASS package is installed as it 
## makes use of the ginv() function. If TRUE, the function
## searches for the inverse of a matrix in the global environment
## comupting the result if it is not found

cacheSolve <- <- function(x, ...) {
  MASStest <<- is.element("MASS", installed.packages()[,1])
  if(MASStest == TRUE) {
    m <- x$getinv()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- ginv(data, ...)
    x$setinv(m)
    m
  }
  else {
    print("Please use install.packages() to install the MASS package")
  }
}
