## The following function creates a list of functions that 
## set, get, set the inverse of the matrix and get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinvmatrix <- function(solve) inv<<- solve
  getinvmatrix <- function() inv
  list(set = set, get = get, setinvmatrix = setinvmatrix, getinvmatrix=getinvmatrix)
}


## The following function returns the inverse of a matrix. It first checks
## to see if the inversematrix is cached, if so it returns the invere matrix else it calculates 
## the inverse of the matrix and sets the inverse of the matrix in the 
## cache by using setinvmatrix function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinvmatrix()
  if (!is.null(inv)) {
    message("getting cached inverse matrix")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinvmatrix(inv)
  inv
}
