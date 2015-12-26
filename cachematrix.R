# Brief introduction:
# Below are two functions that are used to create a special 
# object that stores a numeric matrix and cache's its inverse.

# Description of the first function:
# makeCacheMatrix creates a special "matrix", which is really 
# a matrix containing a function to (first assign variable "a" to makeCacheMatrix object)

# 1. set the value of the matrix: For example, type in "a$set(matrix(1:4,2,2))", 
#    it will assign a special 2*2 matrix to makeCacheMatrix object "a".

# 2. get the value of the matrix: For example: type in "a$get()", 
#    it will return the assigned matrix in makeCacheMatrix object "a".

# 3. set the inverse of the matrix: For example: type in "a$setinverse(solve(a$get()))", 
#    it will assign the inversed 2*2 matrix of matrix 1:4 to makeCacheMatrix object "a".

# 4. get the inverse of the matrix: For example: type in "a$getinverse()", 
#    it will return the assigned inversed matrix in makeCacheMatrix object "a".


makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse = matrix()) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


# Return the cached inverse matrix in object a. If NULL, apple solve() function on the matrix 
# in a, and solve the inverse matrix.

cacheSolve <- function(x) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$setinverse(m)
    m}
