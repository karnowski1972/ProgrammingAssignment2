## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        #This functions creates a list that wraps four functions:
        #1. makeCacheMatrix$Set 
        #2. makeCacheMatrix$get
        #3. makeCacheMatrix$setinv
        #4. makeCacheMatrix$getinv
        
        m <- NULL # mean cache resets
        #1. makeCacheMatrix$Set - sets matrix x cache and resets solved cache
        set <- function(y) {
                x <<- y
                m <<- NULL 
        }
        #2. makeCacheMatrix$get - querries matrix x cache
        get <- function() x
        #3. makeCacheMatrix$setinv - stores the given inverse matrix for future use (<-- assignes value to m of parent environment)
        setinv <- function(solve) m <<- solve
        #4. makeCacheMatrix$getinv - retrieves chached inversed matrix 
        getinv <- function() m
        #Wrappes all four function into a list as output
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        #querries inverse matrix chache stored in m
        m <- x$getinv()
        #test if cache m is not empty, then give message, returns cache and exit function cacheSolve
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        #If there is no inverse matrix cache, retrieves the original matrix to data and
        data <- x$get()
        #calculate inverse matrix to m 
        m <- solve(data, ...)
        #saves the result to inverse matrix cache
        x$setinv(m)
        #returns the results
        message("freshly calculate data")
        m
        
}
