# makeCacheMatrix takes a matrix as input and returns it inverse and 
# saves the inverse as cache. cacheSolve takes a list of functions as input
# and checks the cache for inverse. If inverse is found in cache then it 
# prints the result from the cache or else it calculates the inverse 
# on its own

#  makeCacheMatrix takes matrix as argument and stores it in x
#  set is used to set input arguments for the function
#  get is used to display the inputs
#  setinv sets the inverse of the input matrix
#  getinv displays the inverse
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  setinv <- function(x) m <<- solve(x)
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}
# 
# cacheSolve takes a list of fuctions as arguments
# It checks if the inverse is present in the cache
# If it is present it returns the inverse from the cache or else it 
# calculates the inverse and returns it
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinv(m)
  m
}
