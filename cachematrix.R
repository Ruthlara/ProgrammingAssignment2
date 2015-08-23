# we create the function makecachematrix 
# this function works like a class, it creates a list

makeCacheMatrix <- function(x = matrix()) {
      xinverse <- NULL # this is where the result of inversion is stored
            set <- function(y) {
	  x <<- y
	  xinverse <<- NULL 
      }
      get <- function() 
      setInverse <- function(inverse) xinverse <<- inverse # set the inversed matrix
      getInverse <- function() xinverse # return the inversed matrix
      # return a list that contains these functions, so that we can use
      list(set = set, get = get,
	       setInverse = setInverse,
	       getInverse = getInverse)
  }
  cacheSolve <- function(x, ...) {
      m <- x$getInverse() # get the inversed matrix from object x
      if(!is.null(m)) { # if the inversion result is there
	  message("getting data cached")
	  return(m) # return the calculated inversion
      }
      data <- x$get() 
      m <- solve(data) # we solve it
      x$setInverse(m) # then set it to the object
      m # return the solved result
  }

  test <- matrix(runif(9,1,100),3,3)
    testCached <- makeCacheMatrix(test)
    testInverse <- cacheSolve(testCached)
  
}
## THIS FUNCTION WORKS LIKE A CLASS from now on calculate or retrieve calculated inversion using the cacheSolve function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
}
