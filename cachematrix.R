## This is a memo function version of solve (matrix invert)
 
#makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(mymatrix=numeric()) {
	inverse <- NULL
	set <- function(aMatrix) {
		mymatrix <<-aMatrix
		inverse <<- NULL
	}
	get <- function() mymatrix
	setInverse <- function(anInverse) inverse <<-anInverse
	getInverse <- function() inverse
		list(set = set, get = get,
		setInverse = setInverse,
		getInverse = getInverse)
}


#cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(mymatrix,...) {
        inverse <- mymatrix$getInverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        matrixData <- mymatrix$get()
        inverse <- solve(matrixData)
        mymatrix$setInverse(inverse)
        inverse
}
