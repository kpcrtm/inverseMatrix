#
# makeCacheMatrix:
#
# Create a special matrix object that can be cached.  The <<- operation is 
# used to assign the object to another environment.
#
# Paramater: 
#	x a square invertible matrix
# Returns:
#	list of functions to 
#		- set the matrix
#		- get the matrix
#		- set the matric inverse
#		- get the matric inverse
#
makeCacheMatrix <- function(x = matrix())
{
    inv <- NULL

    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv

    list(set=set, get=get,setinv=setinv, getinv=getinv)
}

#
# cacheSolve:
#
# Computes the matrix inverse or returns the version cached in 
# another environment
#
# Parameter:
#	x the cachable matrix object created in makeCacheMatrix
# Returns:
#	The matrix inverse
#
cacheSolve <- function(x, ...)
{
    inv <- x$getinv()
    
    if (!is.null(inv)) {
        return(inv)
    }

    data <- x$get()
    inv <- solve(data, ...)
    
    x$setinv(inv)

    inv
}


#
# testCache:
#
# This creates a 1000 x 1000 matrix and calculates the time to both
# calculate the matrix inverse and to fetch it from the cache
#
testCache <- function()
{
	# Create a 1000 x 1000 invertible matrix
	m <- matrix(rnorm(1000 * 1000), 1000, 1000)

	# Create the cacheable matrix object
	mobj <- makeCacheMatrix(m)

	# Calculate the matrix inverse
	ptm <- proc.time()
	cacheSolve(mobj)
	t1 <- proc.time() - ptm
	cat("Time to calculate matrix inverse: ", t1[1], "\n")

	# Fetch the matrix inverse from another environment
	ptm <- proc.time()
	cacheSolve(mobj)
	t2 <- proc.time() - ptm
	cat("Time to fetch matrix inverse from cache: ", t2[1], "\n")
}
