## First function creates a list with several values
## The second function calculates the inverse using solve function
## but checks first whether the inverse was already calculated or not
## if it was then it just returns the value that had been calculated if not then it calculates the inverse

## thats the first function that will help to check whether the inverse is already calculated and stored in memory

makeCacheMatrix <- function(x = matrix()) {

  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setSolve <- function(solve) m <<- solve
  getSolve <- function() m
  list(set = set, get = get,
       setSolve = setSolve,
       getSolve = getSolve)
  
}


## this function calculated the inverse matrix using solve but first checks for existence of the m value
## if it exists i.e. is not null then it will not calculate and will just return what is stored in memory
## if it does not exist then it will calculate the inverse of the matrix using solve funcion

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getSolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setSolve(m)
  m
  
}
