## A makeCacheMatrix function creates list of functions
## 1. set - save the matrix in a "cache" and resets inverted matrix
## 2. get - returns cached matrix
## 3. setinverse - saves inverted matrix
## 4. getinverse - returns cahched inverted matrix

## A cacheSolve function checks availability of cached result, and
## return it if get one. If cached result isn't found, the function calculate
## matrix inversion and save it in cache.

## Service function for cacheSolve
## Saves in "cache" source matrix and inverted one

makeCacheMatrix <- function(x = matrix()) {
  inversem <- NULL
  set <- function(y) {
    x <<- y
    inversem <<- NULL
  }
  get <- function() x
  setinverse <- function(solvedinversion) inversem <<- solvedinversion
  getinverse <- function() inversem
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Function for matrix invertion calculation using cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inversem<- x$getinverse()
  if(!is.null(inversem)) {
    message("getting cached data")
    return(inversem)
  }
  data <- x$get()
  inversem <- solve(data, ...)
  x$setinverse(inversem)
  inversem
}
