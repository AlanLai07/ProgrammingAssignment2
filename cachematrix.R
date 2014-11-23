## makeCacheMatrix function is to create a new matrix object with
## accessable sub-functions and cached its inversed matrix. By calling
## makeCacheMatrix, a new matrix object is created.
## cacheSolve function is to use the sub-functions within the matrix
## object to get matrix's inversed matrix if cached, get matrix content,
## and set matrix's inversed matrix. By calling cacheSolve, targeted
## matrix object's inversed matrix is either directly returned if cached
## before, or calculated and cached back then returned.


## makeCacheMatrix function, as introduced similarly in README.md,
## has major ability to create a new matrix object x (terminology
## brought from C++), with elements like inversed matrix "cachedInv",
## a similar user definable constructor function "set()" to create
## empty x and "cachedInv", a "get()" function to return the created
## matrix object content, a "setmean()" function to set "cachedInv"
## matrix by assiging it by argument "inv", and a "getmean()" function
## to get "cachedInv" matrix.

makeCacheMatrix <- function(x = matrix())
{
        ## Judgement of input matrix invertibility
        if(nrow(x) != ncol(x))
        {
                message("Matrix is not Squre and not Invertible!")
                message(c("# of Row is: ", nrow(x), ",    # of Col is: ", ncol(x)))
                message("Please re-input...")
                return()
        }
        
        message("Matrix is Squre and Invertible.")
        message(c("# of Row is: ", nrow(x), ",    # of Col is: ", ncol(x)))
        message("Object created...")
        
        ## Initialize element cachedInv
        cachedInv <- NULL
        
        ## set function to create the new matrix object
        set <- function(y)
        { 
                x <<- y
                cachedInv <<- NULL
        }
        
        ## get function to return the created matrix object
        get <- function()
        {return(x)}
        
        ## setinverse function to assign cachedInv by argument inv
        setinverse <- function(inv)
        {cachedInv <<- inv}
        
        ## getinverse function to return cachedInv
        getinverse <- function()
        {return(cachedInv)}
        
        ## Accessable functions list for $ calling by matrix object
        list(set = set,
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve function, as introduced similarly in README.md,
## is a collection of calling sub-functions defined within
## the matrix object constructed from makeCacheMatrix.
## It first tries to find the cached inverse matrix "cachedInv"
## if possible. If not then it calculates inverse matrix from x
## by solve() function, stores back to "cachedInv", and return
## the calculated inverse matrix

cacheSolve <- function(x, ...)
{
        ## If cachedInv from matrix x is not NULL, inverse matrix has been cached and can be returned
        if(!is.null(x$getinverse()))
        {
                message("Inverse Matrix has been cached before. Now retrieving...")
                return(x$getinverse())
        }
        
        ## Otherwise calculate inverse matrix from matrix x and store it back to cachedInv
        message("Inverse Matrix hasn't been cached before. Now calculating...")
        x$setinverse(solve(x$get()))

        ## Then return the calculated inverse matrix
        return(solve(x$get()))
}
