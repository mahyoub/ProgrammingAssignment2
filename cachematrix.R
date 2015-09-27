

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
      
      get <- function() x
      
      setCache <- function(newCache) cache <<- newCache
      
      getCache <- function() cache
      
      list(set = set, get = get, setCache = setCache, getCache = getCache)

}


## Write a short comment describing this function

cacheSolve   <- function(x, ...) {
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
