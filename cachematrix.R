## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {    
    inv <- NULL             #assigns NULL to a variable within the current environment
    set <- function(y){
      x <<- y               #cache the matrix
      inv <<- NULL          #search for an existing definition of the variable and set to NULL
    }
    get <- function() {x}   #get the matrix cached with set
    setInverse <- function(inverse){inv <<-inverse}   #cache value of the inverse matrix
    getInverse <- function(){inv}    #get the inverse matrix cached with setInverse
    list(set = set, get = get, setInverse =setInverse, getInverse =getInverse)    #creates list to house the four functions)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()     # if an inverse has already been calculated this gets it
  if(!is.null(inv)){        # check to see if cacheSolve has been run before
    message("getting cached data")    
    return(inv)
  }
  mat <- x$get()    # run the getmatrix function to get the value of the input matrix
  inv <- solve(mat, ...)    # compute the value of the inverse of the input matrix
  x$setInverse(inv)    # run the setinverse function on the inverse to cache the inverse
  inv    # return the inverse
        ## Return a matrix that is the inverse of 'x'
}
