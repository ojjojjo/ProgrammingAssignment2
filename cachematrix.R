## Overall, this pair of functions cache the inverse of a matrix.

##The first function, `makeCacheMatrix` creates a special "matrix", 
#  which can cache its inverseis.
# It really is a list containing a function to

#1.  set the value of the matrix
#2.  get the value of the matrix
#3.  set the value of the inverse
#4.  get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() {
    x
  }
  setInverse <- function(inverse) {
    m <<- inverse
  }
  getInverse <- function() {
    m
  }
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}




## This function computes the inverse of the special
##  "matrix" returned by `makeCacheMatrix` above. If the inverse has
##  already been calculated (and the matrix has not changed), then
##  `cacheSolve` should retrieve the inverse from the cache.


## The function "cacheSolve" calculates the inverse of the special "matrix"
##  created with the above function. However, it first checks to see if the
##  inverse has already been calculated. If so, it `get`s the inverse from the
##  cache and skips the computation. Otherwise, it calculates the inverse of
##  the data and sets the value of the inverse in the cache via the `setmean`
##  function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()              #query the x vector's cache
  
  if(!is.null(m)) {                #if there is a cache
    message("getting cached data")
    return(m)                      #just return the cache, no computation needed
  }
  
  data <- x$get()                  #if there's no cache
  m <- solve(data)                 #compute the inverse
  x$setInverse(m)                  #save the result to the cache
  m                                #return the result
}