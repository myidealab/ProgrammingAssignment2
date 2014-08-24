### Caching the Inverse of a Matrix

##  Function 1: "makeCacheMatrix" is a function that makes a 
##  matrix object, whose inverse can be cached.

makeCacheMatrix <- function(x = matrix()) { ## The input for x is a matrix.
    m <- NULL                               ## m stores the matrix; it is 
                                            ## assigned the value of NULL.
    
    set <- function(y) {                    ## The set function sets the value of the matrix.
        x <<- y
        m <<- NULL
    }
    get <- function() x                     ## The get function gets the value of the original matrix.
    setmtrx <- function(mtrx) m <<- mtrx    ## `<<-` operator is used to assign the cacheSolve() value.
    getmtrx <- function() m                 ## The cached value is retrieved.
    list(set = set, get = get,
         setmtrx = setmtrx,
         getmtrx = getmtrx)
}

##  Function 2: "cacheSolve" is a function, that can calculate
##  the inverse of the matrix object, which was created by the 
##  makeCacheMatrix function.
##  If the inverse already exists, then the cacheSolve function 
##  will retrieve it from cache.

cacheSolve <- function(x, ...) {            ## The input for x is a matrix.
    m <- x$getmtrx()                        ## m stores the value of the inverse matrix.
    if(!is.null(m)) {                       ## Conditional statement check if it exists.
        message("getting cached data")      ## A message is displayed if it exists.
        return(m)                           ## The value of the inverse matrix is returned,
                                            ## if it exists.
    }
    data <- x$get()                         ## If the inverse matrix doesn't exist,                
    m <- solve(data, ...)                   ## calculate the inverse matrix using the solve()
    x$setmtrx(m)                            ## function.
    m                                       ## Get the value of the inverse matrix.
}

## Check work
n <- makeCacheMatrix(matrix(1:4, 2, 2))     ## Input test values for the matrix function
n$get()

cacheSolve(n)   # 1st try
cacheSolve(n)   # 2nd try should get from cached data

# Verify results using an matrix calculator
# http://www.bluebit.gr/matrix-calculator/calculate.aspx
