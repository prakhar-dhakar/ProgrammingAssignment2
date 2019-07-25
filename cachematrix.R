
makeCacheMatrix <- function( m = matrix() ) {
    i <- NULL
    set <- function( matrix ) {
            m <<- matrix
            i <<- NULL
    }
    get <- function() {
    	m
    }
    setInv <- function(inverse) {
        i <<- inverse
    }
    getInv <- function() {
        i
    }
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}
cacheSolve <- function(x, ...) {
    m <- x$getInv()
    if( !is.null(m) ) {
            message("trying to get cached data")
            return(m)
    }
    data <- x$get()
    m <- solve(data) %*% data
    x$setInv(m)
    m
}
