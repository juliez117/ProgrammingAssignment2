## Functions do Matrix Inversion

## Function makeCacheMatrix creates a special vector, containing
## 1) set matrix; 2) get matrix; 3) set inverse; 4) get inverse

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y) {
		x <<- y
		i <<- NULL
	}
	get <- function() x
	setinverse <- function(solve) i <<- solve
	getinverse <- function() i
	list(set = set, get = get, 
	    setinverse = setinverse,
	    getinverse = getinverse)
}


## Function cacheSolve calculates the inverse of matrix from above function. 
## If inverse has been calculated, getinverse() from cached
## Otherwise, calculate the inverse and set it in the cached via setinverse()

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if (!is.null(i)) {
		message("getting cached inverse")
		return(i)
	}
	matrix <- x$get()
	i <- solve(matrix, ...)
	x$setinverse(i)
	i
}
