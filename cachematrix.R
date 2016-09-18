## Put comments here that give an overall description of what your
## functions do

## Prepare data matrix

makeCacheMatrix <- function(x = matrix()) {
if(nrow(x) != ncol(x)){
    warning("Cacheable matrix must be square matrix!")
    return
  }
  
  inversedMtrx <- NULL
  set <- function(c) {
    if(nrow(c) != ncol(c)){
      warning("Cacheable matrix must be square matrix!")
      return
    }
    x <<- c
  }
  get <- function() x
  
  setInversedMatrix <- function(invMtx) inversedMtrx <<- invMtx
  getInversedMatrix <- function() inversedMtrx
  
  mtxObj <- list(set = set, get = get, 
       setInversedMatrix = setInversedMatrix,
       getInversedMatrix = getInversedMatrix)
  invisible(mtxObj)
}


## Solve and cache the matrix

cacheSolve <- function(x, ...) {
       solvedMtx <- x$getInversedMatrix()
  if(!is.null(solvedMtx)) {
    message("Getting cached data...")
    return(solvedMtx)
  }
  data <- x$get()
  solvedMtx <- solve(data, ...)
  x$setInversedMatrix(solvedMtx)
  message("First time to set solved matrix data")
  solvedMtx
}
