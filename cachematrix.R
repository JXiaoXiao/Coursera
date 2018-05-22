## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# This function will create an object that stores a matrix and the inverted matrix of that matrix.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {               #it assigns values to x and m, x will be assigned the matrix and m NULL
    x <<- y
    m <<- NULL
  }
  get <- function() x                #It gets the matrix from the parent environment
  setInverse <- function(solve) m <<- solve               #It sets the inverse of m
  getInverse <- function() m                              #It gets the correct value of m through lexical scoping
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)                           #By naming the set = set etc. It will enable us to use the $ 
}                                                         #sign to get the values


## Write a short comment describing this function

# This function retrieves the cached inverted matrix from the makeCacheMatrix environment.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()                #It gets the inverse from the makeCacheMatrix
  if(!is.null(m)) {                  #If the inverse from the makeCacheMatrix isn't NULL then it exists and it will
    message("getting cached data")   #retrieve it from makeCacheMatrix
    return(m)
  }
  data <- x$get()                    #If the inverted matrix doesn't exist it will calculate it by getting the matrix
  m <- solve(data, ...)              #and use the solve() functionon the data. 
  x$setInverse(m)                    #Afterwards it caches the inverted matrix to the makeCacheMatrix Dataset
  m
}

