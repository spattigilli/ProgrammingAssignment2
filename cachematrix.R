## This group of functions provide a solution for caching the matrix result 
## which helps in reducing the computational time when we feel the need for
## calculating the inverse of the same matrix multiple times


## makeCacheMatrix takes a square matrix as input and returns an object which 
## where it stores the original matrix and also encompassess a place holder 
## for inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m_inv <- NULL
  set <- function(y) {
    x <<- y
    m_inv <<- NULL
  }
  get <- function() x
  setmatinv <- function(matrix_inverse) m_inv <<- matrix_inverse
  getmatinv <- function() m_inv
  list(set = set, get = get,
       setmatinv = setmatinv,
       getmatinv = getmatinv)  
}


## cacheSolve takes the object that was returned by makeCacheMatrix function as input
## This function first retreives the inverse matrix from the object. If the object 
## possesses the inverted matrix the function returns the same, in case not the inverse 
## of the matrix is calculated, stored in the object for further use and is returned back

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getmatinv()
  if(!is.null(m)) {
    message("getting cached inverse matrix")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setmatinv(m)
  m
}