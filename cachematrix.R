# we create the function makecachematrix 
# this function works like a class, it creates a list

makeCacheMatrix <- function(x = matrix()) {
  xinv <- NULL # this is where the result of inversion is stored
  set <- function(y) {
    x <<- y
    xinv <<- NULL 
  }
  get <- function() 
    setInv <- function(inv) 
      xinv <<- inv # set the inversed matrix
  getInv <- function() 
    xinv # return the inversed matrix
  # return a list that contains these functions, so that we can use
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}
cacheSolve <- function(x, ...) {
  m <- x$getInv() # get the inversed matrix from object x
  if(!is.null(m)) { # if the inversion result is there
    message("getting data cached")
    return(m) # return the calculated inversion
  }
  data <- x$get() 
  m <- solve(data) # we solve it
  x$setInv(m) # then set it to the object
  m # return the solved result
}

test <- matrix(runif(9,1,100),3,3)
testCached <- makeCacheMatrix(test)
testInv <- cacheSolve(testCached)
  
}
## THIS FUNCTION WORKS LIKE A CLASS from now on calculate or retrieve calculated inversion using the cacheSolve function.
