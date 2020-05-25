##Below 2 functions together can be used to cache the inverse value of a matrix
##into a different environment, so if needed again, instead of recalculating,
##the function will "get" the inverse value from cache. 


## makeCacheMatrix function takes a matrix as input and returns a list of functions
## which set the value of matrix, get the value of matrix,
##set the inverse value of the matrix, get the inverse value of matrix
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(i) inv <<- i
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## cacheSolve function take the output list of functions from makeCacheMatrix function
##and checks if the inverse value is already in cache
##then returns the inverse of the given matrix

cacheSolve <- function(x, ...) {
   inv <- x$getinverse()  # 'inv' is used to store the inverse value of matrix
   if(!is.null(inv)) {
    message("getting cached data")
    return(inv)         #if 'inv' value is found in cache, it is returned
   }
   data <- x$get() 
   inv <- solve(data,...) #'inv' value not found in cache, hence calculated and stored in 'inv'
   x$setinverse(inv) #caculated value of 'inv' is stored in cache
   inv
}
