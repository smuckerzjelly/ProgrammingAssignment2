## The makeCacheMatrix function creates a special "matrix" function that cache its inverse

makeCacheMatrix <- function(x = matrix()) { ##Creates a square invertible matrix
  inv <- NULL 
  set <- function(y) {
    x <<- y
    inv <<- NULL 
    
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  ##The makeCacheMatrix function is really a list 
  ## 1. Set the value of the matrix 
  ## 2. Get the value of the matrix 
  ## 3. Set the value of the inverse 
  ## 4. Get the value of the inverse 
}
## This function computes the inverse of makeCacheMatrix 
## If the inverse was already calculated it will retrieve 
## the inverse from the cache

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data") ##Gets already calculated inverse from the cache
    return (inv)
  }
  mat <- x$get() ##If inverse was not calculated previously it calculates it here 
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
  
  ## Return a matrix that is the inverse of 'x'
}

## Test my function 
## I tested my function in my R console 

> test_matrix <- makeCacheMatrix(matrix(4:8, 2, 2)) ##Declare a matrix in my original function
                                                    ## and name is test_matrix
> test_matrix$getInverse()
NULL
> cacheSolve(test_matrix) #This was the inverse result from the console
      [,1] [,2]
[1,] -3.5    3
[2,]  2.5   -2

