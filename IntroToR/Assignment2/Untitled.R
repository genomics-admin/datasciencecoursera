## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#This function implements 4 basic parts- the set,get, 
#setinverse,getinverse portions
#Not much of the actual portion of the program happens here
#The second function uses the portions defined here to retrieve the necessary values
#Sets the matrix, Gets the matrix, Sets the inverse of the matrix, Gets the corresponding inverse
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
#The function basically checks to see first if a cached value is already present for the given matrix. If present, it retrieves it from the cache itself. Otherwise it computes it before returning
#First query the vector- x's cache
#If present, return it. No further work is needed
#If not present, then compute the inverse here
#Save the result back in x's cache so that it can be used if called again
#Finally, return the result
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}


###Test Input(s) :

###Valid Scenario:

##Set a inversible matrix to m
x <- rbind(c(1, -1/4), c(-1/4, 1))

##Calling makeCacheMatrix with X so that the values gets initialised.
z<- makeCacheMatrix(x)


##Calling cacheSolveMatrix with X(the actual inversible matrix) and Z(the list of vectors from makeCacheMatrix) as parameters
t1 <- 0
t1 <- cacheSolve(z)
print(t1)
##Re calling the cacheSolveMatrix with same matrix as above to show that it returns the cache data
t1 <- 0
t1 <- cacheSolve(z)
print(t1)
##Re calling the cacheSolveMatrix with new inversible matrix to show that it returns the new data accordingly
x <- rbind(c(1, 1/2), c(1/2, 1))
z<- makeCacheMatrix(x)
t1 <- 0
t1 <- cacheSolve(z)
print(t1)
##Re calling the cacheSolveMatrix with same matrix as above to show that it returns the cache data
t1 <- 0
t1 <- cacheSolve(z)
print(t1)


###Invalid Scenario 1: with non inversible matrix
x=rbind(c(3,6,3),c(5,2,1),c(1,2,1))
z<- makeCacheMatrix(x)
t1 <- 0
t1 <- cacheSolve(z)
print(t1)

###Invalid Scenario 2: with non matrix vector
x=c(3,6,3)
z<- makeCacheMatrix(x)
t1 <- 0
t1 <- cacheSolve(z)
print(t1)

###Invalid Scenario 3: with non square matrix
x=rbind(c(3,6,3))
z<- makeCacheMatrix(x)
t1 <- 0
t1 <- cacheSolve(z)
print(t1)