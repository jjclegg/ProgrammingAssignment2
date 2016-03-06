
#In this example we introduce the <<- operator which can be used to assign a value to an object in an environment that is different from the current environment.
#Write the following functions:
#makeCacheMatrix
#This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
     inv = NULL
     set <- function(y) {
          x <<- y
          inv <<- NULL
     }
     get <- function() x
     setinv <- function(inverse) inv <<- inverse
     getinv <- function() inv
     list(set = set, get = get,
          setinv = setinv,
          getinv = getinv)
}

#cacheSolve:
#This function computes the inverse of the special "matrix" returned by makeCacheMatrix
#above. If the inverse has already been calculated (and the matrix has not changed),
#then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
     inv <- x$getinv()
     if(!is.null(inv)) {
          message("getting cached data")
          return(inv)
     }
     data <- x$get()
     inv <- solve(data, ...)
     x$setinv(inv)
     inv
}

a <- matrix(rnorm(9), 3, 3)
myMatrix <- makeCacheMatrix(a)
myMatrix$get()
cacheSolve(myMatrix)
myMatrix$getinv()

