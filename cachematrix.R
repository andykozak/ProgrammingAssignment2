
##makeCachematrix creates special matrix objects for storing inverse of matrix

makeCacheMatrix <- function(mtrx = matrix()) {
  
  ## set the value of the matrix
  imtrx<- NULL
  set <- function(y) {
    mtrx <<- y
    imtrx <<- NULL
  }
  
  ## get the value of the matrix
  get <- function() mtrx
  
  ## set the inverse of the matrix
  setinverse <- function(inverse) imtrx <<- inverse
  getinverse <- function() imtrx
  
  ## process   inverse of the matrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## function cacheSolver calculcates and returns inverse matrix cached by makeCacheMatrix
## if cached inverse is available then its retrieved otherwise its calculated

cacheSolve <- function(mtrx, ...) {
  ## Return a matrix that is the inverse of 'mtrx'
  
  ## get the inverse of the matrix        
  imtrx <- mtrx$getinverse()
  
  ## check if there is the matrix   
  if(!is.null(imtrx)) {
    message("using cached data")
    return(imtrx)
  }
  ## if its not, get the inverse of the matrix   
  imtrx <- solve(mtrx$get(), ...)
  
  ## set the inverse of the matrix 
  mtrx$setinverse(imtrx)
  imtrx
}
