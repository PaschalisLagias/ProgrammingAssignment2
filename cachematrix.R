## Functions in this file can be used to cache the inverse
## of a square invertible matrix.

## makeCacheMatrix: Creates a special "matrix" object that
## sets the matrix elements, gets the matrix elements,
## sets the inverse matrix and gets the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {

## x is the square invertible matrix
## i is the variable to store the inverse of the matrix
	
	i <- NULL
	set <- function(y) {

## The two lines below assign x and i values to objects
## in an environment other than the current one.
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


## cacheSolve function takes as input the output
## of makeCacheMatrix. It takes the special "matrix"
## returned by makeCacheMatrix and it firstly checks
## if the inverse has been calculated. If yes, it retrieves
## the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	i <- x$getinverse()
## if statement to check whether the inverse matrix has
## already been calculated.
	if(!is.null(i)) {
		message ("getting cached data")
		return (i)
	}
	data <- x$get()
	i <- solve(data, ...)
	x$setinverse(i)
## Print inverse matrix
	i
}
