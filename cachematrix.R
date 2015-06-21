## makeCacheMatrix is a function that creates a special matrix, which is
## really a list of functions to
## 1. set ---------- set the value of the matrix 
## 2. get ---------- get the value of the matrix
## 3. setinverse --- set the inverse matrix
## 4. getinverse --- get the inverse matrix
## this resulting list is used as input in cachesolve function in which
## if the inverse of the matrix "x" is calculated if needed.



## makecachematrix is a function with any matrix as argument
## matrix must be square in order to calculate the inverse

makeCacheMatrix <- function(x = matrix()) {
## inv represents the inverse of the matrix entered in the functionction,
## and is first defined as a NULL
    inv <- NULL
## set is a function that asign to x the value of matrix previously defined
## in parent enviroments, if it hasn't been defined previously, the assignment
## takes place in the global enviroment
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
## get returns the value of the matrix to be evaluated
    get <- function() x
## set the inverse of the matrix "x", looks in parent enviroments for
## previously value assignment to in
    setinverse <- function(inverse) inv <<- inverse
## returns the value of the inverse of the matrix "x"
    getinverse <- function() inv
## creates a list with the functions set, get, setinverse and getinverse
## this list input to the cachesolve function
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## uses as input the list returned by the makecachematrix
cacheSolve <- function(x, ...) {
# assign to inv the value stored in the getinverse function
  inv <- x$getinverse()
# if the value of inv is not NULL, get the value previously calculated
    if(!is.null(inv)) {
        message("gettig cached data")
        return(inv)
    }
## if the inv value is null gets the data from the get function
    data <- x$get()
## calculate the inverse of the matrix
    inv <- solve(data, ...)
## set the invers of the matrix
    x$setinverse(inv)
## Return a matrix that is the inverse of 'x'
    inv
}
