## This function creates a matrix and funtions to access matrix data
#extra comment
makeCacheMatrix <- function(x = matrix()){
	m <- NULL
	set <- function(y){
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setInverse <- function(inverse) m <<- inverse
	getInverse <- function() m

	list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## This function stores matrix data in cache if not already there

cacheSolve <- function(x, ...){
	m <- x$getInverse()
	if(!is.null(m)){
		message("getting cached data")
		return(m)
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setInverse(m)
	m
}
