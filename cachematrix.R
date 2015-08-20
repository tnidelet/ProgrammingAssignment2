#This two functions are calculating the inverse of a matrix and put it in the cache. 
#This in order to save time if you have to redo this operation often.

##makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function(){x} 
  setinverse <- function(inverse){m <<- inverse} 
  getinverse <- function(){m} 
  list(set = set, get = get,setinverse = setinverse,getinverse = getinverse)
}

##This function computes the inverse of the special "matrix" returned by 
#makeCacheMatrix above. If the inverse has already been calculated (and the matrix 
#has not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  my_matrix <- x$get()
  m <- solve(my_matrix, ...) #return the inverse of the matrix : "my_matrix"
  x$setinverse(m)
  m
}