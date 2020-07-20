#empty matrix
makeCacheMatrix <- function(x = matrix()) {
        the_inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        #getting the value of the matrix.
        get <- function() x
        #getting value of the inverse.
        setInverse <- function(inverse) the_inv <<- the_inverse
        getInverse <- function() the_inv
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}
#the <<- operator is used to assign a value to an object in an environment different than the current environment.
#be careful when using this type of operator. 
cachesolve <- function(x, ...) {
       
         the_inv <- x$getInverse()
        if(!is.null(the_inv)){
                message("getting cached data")
                return(the_inv)
        }
         #created a random variable named c
         
        c <- x$get()
        the_inv <- solve(c, ...)
        x$setInverse(inv)
        inv
}
