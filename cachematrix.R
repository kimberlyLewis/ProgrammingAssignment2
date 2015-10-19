## The following functions can be used to make and store the inverse
## of a matrix. Repeating a calculation to invert a matrix is both
## time and resource intensive, this storage will allow the user to
## minimize his/her use of resources.

## Creates a "matrix" object that can store (cache) the inverse of
## the given matrix.

makeCacheMatrix <- function(x = matrix())
{
    #initialize
    inverse <- NULL

    #set value of the matrix
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }

    #get the value of the matrix
    get <- function() x

    #set the value of  the inverse
    setInverse <- function(inv) inverse <<- inv

    #get the value of the inverse
    getInverse <- function() inverse

    #return the list of four created functions
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}



## Computes the inverse of the "matrix" created in makeCacheMatrix.
## If this matrix's inverse has been calculated previously, returns
## the stored matrix instead of recalculating.

cacheSolve <- function(x, ...)
{
    #get the inverse
    inverse <- x$getInverse()

    #if previously calculated, return the inverse
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }

    #otherwise, calculate the inverse
    data <- x$get()
    inverse <- solve(data, ...)

    #set the inverse
    x$setInverse(inverse)

    #return the inverse
    inverse
}


## Example matrices that can be inverted
## m1<-matrix(c(1,2,3,4),nrow=2,ncol=2)
## m2<-matrix(c(1,2,3,4,1,6,1,8,9),nrow=3,ncol=3)
## m3<-matrix(c(4,3,3,2),nrow=2,ncol=2)
