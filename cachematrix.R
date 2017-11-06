## The goal of this function is to store the inverse of a matrix in cache
## and retrieve the value from memory when the cacheSolve function is called.
## This is achieved by writing two distinct functions "makecacheMatrix" and "cacheSolve"
## The "makecacheMatrix" function serves as a container while the "cacheSolve" function performs the actual computation

##This first function is the container function that takes the value of the input matrix and passes that value to the
##ca
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y 
    i <<- NULL
  }
  get <- function() x
  setinv <- function(invert) i <<- invert
  getinv <- function() i
  list(set = set, get = get,
       setinv=setinv,
       getinv=getinv)

}


## The second function, i.e "cacheSolve" actually performs the computation to generate the inverse of the matrix
## This function retrieves the matrix passed from makeCacheMatrix and first tests to ensure that it is not a null matrix
## It then checks to make sure that the matrix is a square matrix (i.e nrow==ncol).
## If both conditions are passed, it then moves on to calculating the inverse and returning the result to makeCacheMatrix


cacheSolve <- function(x, ...) {
        i <- x$getinv()
        data <- x$get() ##Retrieve the matrix passed from the container function
        if(!is.null(i)){
          message("getting cached data")
          return(i)
        }
        if (nrow(data)!=ncol(data)) {
          message("The matrix provided must be a square matrix")
          return()
        }
        
        i <- solve(data,...)
        x$setinv(i)
        i #returning a matrix that is the inverse of "x"
}
