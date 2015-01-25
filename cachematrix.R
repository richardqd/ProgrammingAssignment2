##Module has two operations, one to create a cacheMatrix object and
##another to calculate a cacheMatrix's inverse.
##Special feature of the latter is that it will cache the inverse
##Useful for repeated calculation especially for large matrices
##Note key assumption, matrix is invertable - ie, square and det != 0

##Creates a special matrix object that has getters and setters
##For a matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
   inv <- NULL
   set <- function(y) {
   	x <<- y
        inv <<- NULL
   }

  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## This function derives the inverse of the supplied
## matrix x, which must have been created by makeCacheMatrix
## - also defined in this module. Note that it assumes det !=0
## and that the matrix is square. Failure will result otherwise
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if (!is.null(inv)) {
    #Indicates that the matrix inverse has been cached
    message("cached inverse")
    return (inv)
  }
  
  #Not in the cache, so we'll caculate it
	raw <- x$get()
  
  #Inverse calculated by solving a %x% = b, where a is the input matrix
  #and b is the identity matrix
  inv <- solve(raw, diag(dims[1]))
  
  #assuming successful solve, we'll cache the result
  x$setinv(inv)
  
  #return the result
  return(inv)  
}
