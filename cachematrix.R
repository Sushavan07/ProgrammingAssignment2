#Both of these functions are designed to 
#1) invert a matrix 
#2) check to see if this inversion has already been done and if it has to print that cached result. 
#The objective is to avoid running uneccesary matrix inversions

#makecachematrix creates a list, the list contains four functions: compute the value of the matrix, 
#get the value of the matrix, compute the inverse of the matrix, get the inverse of the matrix

makecachematrix <- function(x = matrix()) {
  m <- NULL
  compute <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  computeinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(compute = compute, get = get,
       computeinverse = computeinverse,
       getinverse = getinverse)
}  


#cachesolve computes the inverse of the matrix created above, however, 
#first it checks to see if this has already been computed, 
#if it has then it will return this cached value

cachesolve <-function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$compute(m)
  m
}
