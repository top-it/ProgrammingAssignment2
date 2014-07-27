## The goal on the two functions makeCacheMatrix and cacheSolve is to speed the inversion of matrix
## insteed of calling solve directly  makeCacheMatrix creat a special object who can
## cache the result so it ca be reused in subsequent calls

## makeCacheMatrix create the special object that can a be used by  cacheSolve

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInv <- function(inverse) inv <<- inverse
  getInv <- function() inv
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## cacheSolve returns the inverse of x
## of the inverse is already in cach it return it
## otherwise it call solve store the result in cach and returns it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  inv <- x$getInv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInv(inv)
  inv
  
  
}
