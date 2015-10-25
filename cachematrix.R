## The first of the two functions below contains four functions to create and store a matrix 
## and its inverse matrix in cache plus functions to retrieve both.
## By making use of the lexical scoping of R, this stored result in cache can then be used by
## the second function. A matrix is passed on to the first function to have the inversed calculated
## and stored. Once this is done the result in cache can be re-used over and over, without needing
## to re-calculate it everytime.

## The function 'makeCacheMatrix' contains four functions that in the end are joined within a list.
## Two functions are about a matrix within the cache - one to have the matrix stored, one to have
## it retrieved.
## Two functions are about calculating the inverse of the matrix- one for the actual calculation,
## and storing of the result within cache, one for retrieving the result from cache.
## By joining the funtions in a list, each of them can be called independently and directly.

makeCacheMatrix <- function(x = matrix()) {
  im <- NULL
  set <- function(y) {    ## This function ensures that the matrix is stored in cache.
    x <<- y               ## initialisation of the matrix in cache. x is in cache, 
                          ## y is the matrix passed on from the call of the function. 
                          ## The matrix in cache (x)is assigned the matrix from the call (y).
    im <<- NULL           ## Initialisation of the inversed matrix (im) in cache. 
                          ## It will start at NULL until it is set by calling the setSolve() function
                          ## This initialisation is needed for the check within the cacheSolve() function.
                          ## Without this the check within cacheSolve will fail when used for the first time.
  }
  get <- function() x     ## This function retrieves the matrix from cache. Since the variable x
                          ## will not be found within this environment, R will search for it in
                          ## the parent environment. In the end it will be found in cache.
  setSolve <- function(solve) im <<- solve ## This function calculates the inverse of the matrix
                                           ## which is passed on and assigns this to im in cache
  getSolve <- function() im                ## This function retrieves the inverse matrix.
  list(set = set, get = get,  ## by creating this list, the four functions can be called independently
       setSolve = setSolve,   ## from each other, and directly.
       getSolve = getSolve)
}


## The following function 'cacheSolve' calculates the inverse of the special "matrix" created with 
## the above function. However, it first checks to see if the inverse has already been calculated. 
## If so, it `get`s the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the matrix given within the argument 
## and sets the value of the inversed matrix in the cache via the `setSolve`.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

  im <- x$getSolve()      ## retrieve the value of the inversed matrix stored in the cache
  if(!is.null(im)) {      ## check if the matrix in the cache has previously recieved 
                          ## a value. When so (is not NULL) it can simply be retrieved 
                          ## instead of having it recalculated.
                       
    message("getting cached data") ## to inform that the value is retrieved from cache
    return(im)                     ## the stored inversed matrix is given as a result of the function
  }
  data <- x$get()         ## When the inversed matrix is not yet available, it must still be
                          ## calculated. For that it must first be retrieved. 
  im <- solve(data, ...)  ## Then the actual computation of the inverse takes place.
  x$setSolve(im)          ## The result of the caclulation of the inversed matrix is stored in 
                          ## cache after all.
  im                      ## the inversed matrix is given as the result of the function.
  }


 