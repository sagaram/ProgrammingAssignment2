# ProgrammingAssignment2
makeCacheMatrix <- function(x = matrix()) {
  
  xinv <- NULL # this is where the result of inversion is stored
  
  set <- function(y) {
    x <<- y
    xinv <<- NULL # it also initialises xinv to null
  }
  
  get <- function() x # return the input matrix
  setInv <- function(inv) xinv <<- inv # set the inversed matrix
  getInv <- function() xinv # return the inversed matrix
  
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}
cacheSolve <- function(x, ...) {
  m <- x$getInv() 
  if(!is.null(m)) { # if the inversion result is there
    message("getting cached data")
    return(m) # return the calculated inversion
  }
  data <- x$get() 
  m <- solve(data) 
  x$setInv(m) 
  m 
}


test <- matrix(runif(9,1,100),3,3)

testCached <- makeCacheMatrix(test)

testInv <- cacheSolve(testCached)
testInv <- cacheSolve(testCached)
testInv <- cacheSolve(testCached)
testInv <- cacheSolve(testCached)
testInv <- cacheSolve(testCached)
