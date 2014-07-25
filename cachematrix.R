######
#Create a matrix that can cache its inverse
######

makeCacheMatrix <- function(Mtrix = matrix()) {
        ### initial the matrix Object
        mtrixInverse <- NULL
                                #set matrix
        set <- function(y) {
                Mtrix <<- y
                mtrixInverse <<- NULL
        }
                                #get matrix
        get <- function() Mtrix
                                #  set the inverse 
        setInverse <- function(inverse) mtrixInverse <<- inverse 
                                # get function to return the cached inverse
        getInverse <- function() mtrixInverse 
                                #list the available function in makeCacheMatrix
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


#### This will return the inverse of the matrix 
#### If the inverse has already been calc then the cacheSolve
#### will retrieve & return the inverse from cache

cacheSolve <- function(x, ...) {

	   ## Return a matrix that is the inverse of 'x' 
         ## check if the inverse of the matrix exists in cache

          invMtrx <- x$getInverse()

         # if inverse of the matrx exists, 
         #   then get the matrix inverse from cache and return

          if(!is.null(invMtrx)) {
                    message("getting cached data")
                    return(invMtrx)
            }
         # if the cache is empty , then proceed
         # x$get() gets the matrix 

            data <- x$get()

         # function solve returns the inverse
 
            invMtrx <- solve(data)

         # set the inverse in cache

            x$setInverse(invMtrx)

         # return the inverse
            invMtrx
}

