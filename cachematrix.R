

## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  
      cache <- NULL
      set<-function(z){
        x<<-z
        cache <- NULL
      }
      # returns the stored matrix
      get <- function() x
      # cache the given argument 
      setCache <- function(solve) cache <<- solve
      # get the cached value
      getCache <- function() cache
      # return a list
      list(set = set, get = get, setCache = setCache, getCache = getCache)

}


## Write a short comment describing this function

cacheSolve   <- function(x, ...) {
##cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), 
##then cacheSolve should retrieve the inverse from the cache.
 ## Return a matrix that is the inverse of 'x'
  cache <- x$getCache()
  if(!is.null(cache)) {
    message("loading cached data")
    return(cache)
  }
  data <- x$get()
  cache <- solve(data, ...)
  x$setCache(cache)
  
  cache
}
