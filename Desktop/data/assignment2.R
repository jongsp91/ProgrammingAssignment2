## makeCacheMatrix

makeCacheMatrix <- function(x = matrix()){
  i <- NULL
  
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  get <- function()x
  setinv <- function(inv) i <<- inv
  
  getinv <- function()i
  
  list(set = set, get = get, setinv = setinv, getinv = getinv )
  
}

cacheSolve <- function(x, ...){
  i<- x$getinv()
  
  if(!is.null(i)){
    message("cached data")
    return(inv)
    
  }
  
  mat.data = x$get()
  inv = solve(mat.data, ...)
  
  x$setinv(inv)
  
  return(inv)
}
