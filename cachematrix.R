## Create a matrix and store, compute & cache inverse 

makeCacheMatrix <- function(x = matrix()) { #define the default value of x as an emply matrix
        i <- NULL                           #set i as null, initializing as an object within makeCacheMatrix
        set <- function(y){                 #define the set function 
                x <<- y                     #assign the input argument to the x object in the parent env 
                i <<- NULL                  #assign the value of NULL to the i object in the parent env 
        }
        get <- function() x                 #define the getter for the matrix x 
        setinverse <- function(solve) i <<- solve          #define the setter for the inverse i
        getinverse <- function() i          #define the getter for the function inverse i
        list(set = set,                     #define the names for the functions 
             get = get, 
             setinverse = setinverse, 
             getinverse = getinverse)
}


## Returns a matrix that is the inverse of 'x', returns from cache if inverse has previously computed 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()              
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
