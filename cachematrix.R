## Programming Assignment : Caching the Inverse of a Matrix
## Course Era Registrant : Ranjana R

## Functions to calculate the inverse of a inversible matrix with non zero determinant and store in cache with write and read modes

makeCacheMatrix <- function(x = matrix()) 
{
    invmatrix <- NULL
	set <- function(y) 
	{
          x <<- y
          invmatrix <<- NULL
	}
  
	get <- function() x
	setinverse <- function(inverse) invmatrix <<- inverse
	getinverse <- function() invmatrix
	list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Check for cached data for the available inverse value, if not compute using solve() function

cacheSolve <- function(x, ...) 
{
        ## Function to check cached data, if round return the value, else compute using solve() function
	invmatrix <- x$getinverse()
	if (!is.null(invmatrix)) 
	{
          message("getting cached data")
          return(invmatrix)
	}
	data <- x$get()
	invmatrix <- solve(data, ...)
	x$setinverse(invmatrix)
	invmatrix
}
