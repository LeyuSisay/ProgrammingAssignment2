## Put comments here that give an overall description of what your
## functions do



## Uses attributes to store the closure functions: getinversed, setinversed.
##getinversed: will make sure if there are any changes and invalidates the cache if so and returns the stored value
##setinversed: will set cache value in the captured variable
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  original <- x
  
  attr(x,'getinversed') <- function(nx){
    if(!identical(original,nx)){
      inv <<- NULL
      original <<- nx
    }
    inv
  }
  attr(x,'setinversed') <- function(val){
    inv <<- val
  }
  x
}

## Resolves the cache and if cache is null or invalidated, will compute the inverse and set the matrix inverse
cacheSolve <- function(x, ...) {
  inversed <- attr(x,'getinversed')(x)
  
  if(!is.null(inversed)){
    message("getting cached data")
    return(inversed)
  }
  inversed <- solve(x)
  attr(x,'setinversed')(inversed)
  inversed
}
