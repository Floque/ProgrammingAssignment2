## Matrix inversion is usually a costly computation and there may be some
## benefit to caching the inverse of a matrix rather than computing it repeatedly

## Below you'll find two functions, that are used  to create special object, 
## that stores a matrix and caches its inverse. 

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        
        
        
        M<-NULL
        set <- function(y) {
                x <<- y
                M <<- NULL
        }
        get <- function() x
        setinverse <- function(inv) M <<- inv
        getinverse <- function() M
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
        
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        
        M <- x$getinverse()
        if(!is.null(M)) {
                message("getting cached data")
                return(M)
        }
        data <- x$get()
        M <- solve(data, ...)
        x$setinverse(M)
        M
}


### Testing my Functions

test_matrix <- makeCacheMatrix(matrix((1:4),2,2))

test_matrix$get()

test_matrix$getinverse()

cacheSolve(test_matrix)

cacheSolve(test_matrix)

test_matrix$getinverse()

#### test if solve brings the same result

solve(matrix((1:4),2,2))

#### allright, same result

set.seed(123)

test_matrix$set(matrix(rnorm(100),10,10))

test_matrix$getinverse()

cacheSolve(test_matrix)

cacheSolve(test_matrix)

test_matrix$getinverse()

#### test if solve brings the same result

set.seed(123)

solve(matrix(rnorm(100),10,10))

#### allright, same result



