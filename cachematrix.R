# Function makeCacheMatrix 
# 1. set a matrix
# 2. get the matrix
# 3. set the iverse of the matrix
# 4. get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    cm <- NULL
    set <- function(y) {
        x <<- y
        cm <<- NULL
    }
    get <- function() x
    setInverse <- function(solve) cm <<- solve
    getInverse <- function() cm
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


# Function cacheSolve
# if the inverse of the matrix is set, return the inverse
# else inverse (solve() the matrix

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    cm <- x$getInverse()
    if(!is.null(cm)) {
        message("getting cached data")
        return(cm)
    }
    cd <- x$get()
    cm <- solve(cd, ...)
    x$setInverse(cm)
    cm
}
