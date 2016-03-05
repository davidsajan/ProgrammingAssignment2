# This program is used to calculate the inverse if not previosly
# calculated and use cache value if previously done. 

# create a matrix based on the user input, preparation 
# for calculating matrix/cache etc.
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

# display the cache value or calculate the inverse of matrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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