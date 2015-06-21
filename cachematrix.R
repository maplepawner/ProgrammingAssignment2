#These functions "cache" (reserve/store) potentially time-consuming computations so that your 
#computer does not need to compute the same thing over and over.

#This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL # m is the cache holding the inverse
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse) #when we assign makeCacheMatrix to an object,
    #the object has all 4 functions
}

#This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), then cacheSolve 
#should retrieve the inverse from the cache.  

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    else {
        data <- x$get()
        m <- solve(data)
        x$setinverse(m)
        m
    }
}
