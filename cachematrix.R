## makeCacheMatrix sets up the special "matrix" that is really a list containing:
##  1. set the matrix value
##  2. get the matrix value
##  3. set the matrix inverse
##  4. get the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) {
    i <<- solve
  }
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve will attempt to perform the matrix inverse operation. Two conditions exist:
##  1. If the inverse already exists, then the function will retrieve that inverse
##  2. If the inverse does not exist, then the function will perform the operation

cacheSolve <- function(fList) {
  ## Return a matrix that is the inverse of 'x'
  i <- fList$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- fList$get()
  i <- solve(data)
  fList$setinverse(i)
  i
}
