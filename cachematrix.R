## Put comments here that give an overall description of what your
## functions do

## MakeCacheMatrix takes a matrix as an argument and makes sure it is cached in another environment in case it is set, or get it if it is already set or get its inverse if this matrix inverse was already calculated or set this inverse in case if it was firstly calculated

makeCacheMatrix <- function(x = matrix()) {
S=NULL
set <- function(y) {
  x <<- y
  S <<- NULL
}
get <- function() x
setINV <- function(INV) S <<- INV 
getINV <- function() S
list(set = set, get = get,
     setINV = setINV,
     getINV = getINV)
}


## This finction check first cached calculation if the inverse was previously calculated and cached, if not, it will calculate the inverse and put it in set inverse location so next time it is cached and not calculated from scratch

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  S <- x$getINV()
  if(!is.null(S)) {
    message("getting cached data")
    return(S)
  }
  data <- x$get()
  S <- solve (data)
  x$setINV(S)
  S
}
