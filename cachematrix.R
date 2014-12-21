## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
##            If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache

## Please note that these functions would only work when given metrix is reversible,else it fails


# This function gets inverse matrix and sets the inverse matrix with super assignemtn operator <<-
makeCacheMatrix <- function(x = matrix()) {
  inv.matrix <- NULL
  set <- function(y) {
    x <<- y
    inv.matrix <<- NULL
  }
  get <- function() x
  # Once it sets the inverse of matrix to inv.metrix , change gets applied to the containing environment
  # so scope of this variable gets applied to outside its defination, 
  # thus inv.matrix has a set value in the global environment 
  setinverse <- function(inv) inv.matrix <<- inv 
  getinverse <- function() inv.matrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


# This function gets the metrics input and checks if inverse is already being calculated or not 
# (It tries to get the inverse from above function)
# If it finds the inverse , it takes it from cache, else it calculates the inverse using setinverse() function

cacheSolve <- function(x, ...) {
  inv.matrix<- x$getinverse()
  if(!is.null(inv.matrix))
  {
    message("getting cached inverse")
    return(inv.matrix)
  }
  data<-x$get()
  inv.matrix<-solve(data)
  x$setinverse(inv.matrix)
  inv.matrix  
}

#Some Sample runs for the code are:
#z <- matrix(c(4,2,7,6), nrow = 2, ncol = 2)
#a<- makeCacheMatrix(z)
#a$get()
#a$getinverse()
#cacheSolve(a)
#a$getinverse()
#cacheSolve(a)

#z<-matrix(c(1,2,3,4),2,2)
#a$set(z)
#a$get()
#a$getinbverse()
#cacheSolve(a)
#a$getinverse()
#cacheSolve(a)
#a$getinverse()
#a$getinverse()

