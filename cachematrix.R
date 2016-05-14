# makeCacheMatrix is a function and its puspose is to store a martix and a cached value of the inverse of the
# set           set the value of a matrix
# get           get the value of a matrix
# cacheinverse  cache value -> inverse of the matrix)
# getinverse    cache value (inverse of the matrix)

makeCacheMatrix <- function(x = matrix()) 
        {
                # Setting the initial cache value to null
                m <- NULL
                # storing the matrix in 
                set <- function(y) 
                        {
                                x <<- y
                                # clearing cache as a value is assigned
                                m <<- NULL
                        }
                # returning the matrix
                get <- function() x
                # store the arguments
                setinverse <- function(inverse) m <<- inverse
                # get the cache value
                getinverse <- function() m
                list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
        }
## This function will calculate the inverse of the matrix from makeCacheMatrix
cacheSolve <- function(x, ...) 
        {
                ## Return a matrix that is the inverse of 'x'
                m <- x$getinverse()
                if(!is.null(m)) 
                        {
                                message("getting cached data")
                                return(m)
                        }
                data <- x$get()
                m <- solve(data, ...)
                x$setinverse(m)
                m
        }