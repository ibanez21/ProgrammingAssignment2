makeCacheMatrix <- function(m = matrix())  {
    inv <- NULL
    set_matrix <- function(y)  {
        m <<- y
        inv <<- NULL
    }
    get_matrix <- function() m
    set_inverse <- function(solve) inv <<- solve
    get_inverse <- function() inv
    list(set_matrix=set_matrix, 
         get_matrix=get_matrix, 
         set_inverse=set_inverse, 
         get_inverse=get_inverse)
}

cacheSolve <- function(x, ...)  {
    inv <- x$get_inverse()
    if(!is.null(inv))  {
        message("getting cached data")
        return(inv)
    }
    data <- x$get_matrix()
    inv <- solve(data, ...)
    x$set_inverse(inv)
    inv
}