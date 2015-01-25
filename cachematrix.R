## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix makes a spiecial "matrix" that stores matrix and has a place to store its inverse
## this function has some "methods":  that can return the stored matrix, return its inverse (or NULL)
## or set the matrix (or update it) or set  the matrix inverse

makeCacheMatrix <- function(x = matrix()) { ## the default argument is empty matrix 
 
  inv <- NULL ## create an "empty" inverse of the matrix 'x'
  
  set <- function(y) { ##  function that sets (updates) the matrix 'x' in the special matrix
    x <<- y  ## set (update) the stored matrix in 'x'
    inv <<- NULL ## delete the cached inverse
  }
 
  get <- function() x ## function that returns the matrix from the special matrix
 
  setinverse <- function(inverse) inv <<- inverse ## function that sets the inverse matrix in the special matrix
 
  getinverse <- function() inv ## function that returns the inverse matrix (or NULL)
 
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse) ## return the special matrix i.e. the list of "methods" and its attributes (matrix 'x and inverse matrix 'inv')

} ## end of the makeCachefunction


cacheSolve <- function(x, ...) { ## 'x' is a special matrix 
  
  inv <- x$getinverse() ## get the stored inverse of the matrix in 'x' or NULL

  if(!is.null(inv)) { ## if the inverse was cached (so it's not NULL)
    message("getting cached inverse") ## write the massege the inverse was cached
    return(inv) ## Return the matrix that is the inverse of 'x' (the inverse was cached)
  } 
  
  ## otherwise
  data <- x$get() ## get the stored matrix in 'x'
  inv <- solve(data, ...) ## find its inverse
  x$setinverse(inv) ## cache it
  inv  ## Return the matrix that is the inverse of the stored matrix in 'x'

}
