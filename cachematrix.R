## Functionality allowing inverse of invertible matrices to be cached

## MakeCacheMatrix function initialises the cacheMatrix object, producing a list 
## containing an invertible matrix and functionality to get and set the matrix
## and inverse, and cache the inverse

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {	##after object is initialised can be
		x <<- y		##reset using set(y).  Also resets
		inv <<- NULL	##inverse matrix to NULL
	}
	get <- function() x	##get matrix
	
	setinverse <- function(inverse) inv <<- inverse	##set inverse
	##note that there is no check that this user-provided inverse is correct
       
	getinverse <- function() inv  ##returns inverse
	list(set = set, get = get, 
		setinverse = setinverse, 
		getinverse = getinverse)
}


## cacheSolve returns the cached inverse, if available.  If the inverse is not
## already cached then it calculates the inverse and caches it

cacheSolve <- function(x, ...) {
	inv <- x$getinverse()	##get cached inverse
	if(!is.null(inv)) {	##if cached inverse isn't NULL then return this
		message("getting cached data")
		return(inv)
	}
	data <- x$get()		##otherwise calculate inverse of the matrix
	inv <- solve(data, ...)
	x$setinverse(inv)		##and store in cache
	inv
}

