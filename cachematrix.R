makeCacheMatrix <- function(x = matrix()) {
        matInv <- NULL
        set <- function(y) {
                x <<- y
                matInv <<- NULL
        }
        get <- function() x
        setMatInv <- function(inverse) matInv <<- inverse
        getMatInv <- function() matInv
        list(set = set,
             get = get,
             setMatInv = setMatInv,
             getMatInv = getMatInv)
}

cacheSolve <- function(x, ...) {
        matInv <- x$getMatInv()
        if (!is.null(matInv)) {
                message("getting cached data")
                return(matInv)
        }
        mat <- x$get()
        matInv <- solve(mat, ...)
        x$setMatInv(matInv)
        matInv
}