#Make a vector (makeCacheMatrix) that contains the matrix

#The vector creates a list containing a function to 
  #Set the value of the matrix
  #Get the value of the matrix
  #Set the value of inverse of the matrix
  #Get the value of inverse of the matrix
makeCacheMatrix <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}
#The following function returns the inverse of the matrix
#The first step is to check if the inverse has been computed
#If it has then the results will be displayed and the skips the computation
#If it does not, then it computes the inverse and set the value
cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- inverse(data, ...)
  x$setinverse(m)
  m
}