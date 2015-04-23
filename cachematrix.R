
makeCacheMatrix <- function(x = matrix()) {

	xinv = NULL
	
	get <- function()x
	setinv <- function(inverse) xinv <<- inverse
	getinv <- function()xinv

	list(set = set, get = get, setinv = setinv, getinv = getinv)

}

cacheSolve <- function(x) {
	
	inverse <- x$getinv()

	if(!is.null(inverse)) {
		message("getting cached data")
		return(inverse)
	}

	data <- x$get()
	inverse <- solve(data)
	x$setinv(inverse)
	inverse

}

