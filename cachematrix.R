## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inv2) inv <<- inv2 #inv2 is fed in while inv is stored
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinv() #reads the stored inverse and stored into i
  if(!is.null(i)) { #code that checks whether we've cached something before
    message("getting cached data")
    return(i) #returns the cached inverse and ends
  }
  data <- x$get()
  i <- solve(data,...) #solves one time the inverse
  x$setinv(i) #stored inverse into cache
  i
}
