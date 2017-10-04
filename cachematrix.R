## makeCacheMatrix creates a special Matrix capable of caching it's inverse value
## cacheSolve calculate the inverse of the matrix created by makeCacheMatrix and cache the result


## Creates a matrix with getters and setters for it's main and cache values
makeCacheMatrix <- function(m = matrix()){
  ## Creating the cache variable for future use
  cachedMatrix <- NULL
  
  ## getter and setter of the matrix value
  get <- function() m
  set <- function(newMatrix)  {
    m <<- matrix
    cachedMatrix <<- NULL
  }
  
  ## getter and setter of the cached value (inverted matrix)
  getCache <- function() cachedMatrix
  setCache <- function(newCacheValue) cachedMatrix <<- newCacheValue

  ## return the getters and setter created above
  list(get = get, set = set, getCache = getCache, setCache = setCache)
}

## Calculates the inverse of a matrix and cache the result.
## If there is already a cached value, returns it instead
cacheSolve <- function(cacheableMatrix, ...) {
  ## try to get the cached matrix
  invertedMatrix <- cacheableMatrix$getCache()
  
  ## if there is a cached value, returns it...
  if(!is.null(invertedMatrix)){
    message("Getting cached inverted matrix")
    return(invertedMatrix)
  }
  
  ## ...otherwise, calculate the inverse of the matrix and caches it before returning
  data <- cacheableMatrix$get()
  invertedMatrix <- solve(data, ...)
  cacheableMatrix$setCache(invertedMatrix)
  invertedMatrix
}