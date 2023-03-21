makeVector <- function(x = numeric()) {
  ## This function creates a special "vector", which 
  ## is really a list containing a function to
  ## 1. set the value of the vector
  ## 2. get the value of the vector
  ## 3. set the value of the mean
  ## 4. get the value of the mean
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}

cachemean <- function(x, ...) {
  ## This function calculates the mean of the special "vector" 
  ## created with the above function. However, it first checks to see if
  ## the mean has already been calculated. If so, it gets the mean from 
  ## the cache and skips the computation. Otherwise, it calculates the mean 
  ## of the data and sets the value of the mean in the cache via the 
  ## setmean function.
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}

makeCacheMatrix <- function() {
  ## This function creates a special "matrix" object that can cache its inverse.
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

cacheSolve <- function(){
  ## This function computes the inverse of the special "matrix" 
  ## returned by makeCacheMatrix above. If the inverse has already 
  ## been calculated (and the matrix has not changed), then the 
  ## cachesolve should retrieve the inverse from the cache.
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}