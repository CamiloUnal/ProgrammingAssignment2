## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to
## 1 set the value of a matrix
## 2 get the value of a matrix
## 3 set the value of the inverse of a matrix
## 4 get the value of the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x   # función para obtener una matriz x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m  # función para invertir la matriz
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## Write a short comment describing this function

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve 
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    m <- x$getsolve()
    if(!is.null(m)) {   # comprueba si la inversa es NULL
        message("getting cached data")
        return(m)  # devuelve valor inverso
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m   # # Devuelve una matriz que es la inversa de 'x'
}
