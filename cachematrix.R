## makeCacheMatrix will not modify the Matrix passed and other code can still use the returned object as a regular matrix object which is a plus
## It stores tehe closure functions as asstributes
## getinversed: Will invalidate the cache if any changes were detected; otherwise returns the cached inverse
## setinversed: Will set cache value in the captured variable
makeCacheMatrix <- function(x = matrix()) {
  #Initialize the closed values
  inv <- NULL
  original <- x
  
  attr(x,'getinversed') <- function(nx){
	#Was the matrix modified? If so, invalidate the stored values and change the x value to the new x, this will force the cacheSolve function to recompute the inverse
    if(!identical(original,nx)){
      inv <<- NULL
      original <<- nx
    }
	#Return the inversed value (will be NULL for first time use and also when matrix changes were detected)
    inv
  }
  attr(x,'setinversed') <- function(val){
    inv <<- val
  }
  x
}

## Resolves the cache and if cache is null or invalidated, will compute the inverse and set the matrix inverse
cacheSolve <- function(x, ...) {
  #Get cahched inverse matrix from x
  inversed <- attr(x,'getinversed')(x)
  
  #Cache has value
  if(!is.null(inversed)){
    message("getting cached data")
    return(inversed)
  }
  #Compute the inverse of x here and set it so that it will be used in subsequent functions
  inversed <- solve(x)
  attr(x,'setinversed')(inversed)
  inversed
}