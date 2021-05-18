
# This function accepts a matrix as it arguments and contains 
# other functions that get and set the matrix inverse that will
# be calculated in the secondary function below

makeCacheMatrix <- function(m = matrix())  {
    inv <- NULL
    # the function set_matrix will take as its argument a matrix, and 
    # will update the value returned when get_matrix() is called
    set_matrix <- function(y)  {
        m <<- y
        inv <<- NULL
    }
    # get_matrix returns the matrix associated with the object
    get_matrix <- function() m
    # set_inverse accepts a matrix as an argument, and stores the 
    #result in inv. It is not meant to be called from the object, rather
    # set_inverse is called in the function below, and the result is placed
    # in the inv variable above.
    set_inverse <- function(solve) inv <<- solve
    # get_inverse returns the inverse of the matrix argument m.
    get_inverse <- function() inv
    # return list of functions for object
    list(set_matrix=set_matrix, 
         get_matrix=get_matrix, 
         set_inverse=set_inverse, 
         get_inverse=get_inverse)
}

# This function solves for the inverse of a matrix provided as an
# attribe of an R object x, provided the inverse has not already been
# calculated. If the inverse has been calcualated, the function returns 
# the inverse from cache

cacheSolve <- function(x, ...)  {
    # attempt to retrieve inverse matrix from object x. If inverse is
    # present, return it from cache without recalculating.
    inv <- x$get_inverse()
    if(!is.null(inv))  {
        message("getting cached data")
        return(inv)
    }
    # here, the inverse has not been calculated, so we do so by using the 
    # solve() function, and storing the result in inv variable, which will 
    # be stored in the x object via the set_inverse() functio above.
    data <- x$get_matrix()
    inv <- solve(data, ...)
    x$set_inverse(inv)
    inv
}