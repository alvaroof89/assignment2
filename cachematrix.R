makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(x) {
    z <- solve(x)
    inverse <<- z
  }  
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
  temp <- x$getinverse()
  if(!is.null(temp)) {
    message("getting inverse matrix")
    return(temp)
  }
  data <- x$get()
  temp2 <- solve(data)
  x$setinverse(temp2)
  temp2
}



