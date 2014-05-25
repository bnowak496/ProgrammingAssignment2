## cache matrix will solve for the inverse of the square matrix passed. 
## it will cache previously solved solutions to speed up common problems. 

## makeCacheMatrix creates an environment to cache previously solved
## matries.
## run with: 
## x<- makeCacheMatrix(square_matrix)
## i<- cacheSolve(x)

# a <- replicate(100,rnorm(100))
#> x <- makeCacheMatrix(a); ptm <- proc.time(); i<- cacheSolve(x); proc.time() -ptm
#  user  system elapsed 
# 0.002   0.000   0.001 
# > ptm <- proc.time(); i<-cacheSolve(x); proc.time() -ptm
# getting cached data
# user  system elapsed 
# 0       0       0 

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL 
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInverse <- function(inv) i <<- inv
  getInverse <- function() i
  list(set = set, get = get,
       setInverse = setInverse, 
       getInverse = getInverse)
}


## This function will return the inverse of a square matrix 
## The function assumes that the matrix is square and does not
## catch errors if the matrix is not square

## cacheSolve will cache previously solved inverses 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getInverse() 
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  # wasn't found in cache so calculate 
  data <- x$get()
  i <- solve(data,...)
  # update the cache 
  x$setInverse(i)
  # return the inverse
  i
}
