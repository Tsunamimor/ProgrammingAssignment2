## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {     
        ##The formals part of the function declaration defines the default value of x as an empty matrix
        
        i <- NULL				
                ##i is cleared from the cache
        
        set <- function(y) {
                x <<- y                         
                        ##Assign the input argument to the x object in the parent environment
                i <<- NULL                      
                        ##Assign the value of NULL to the i object in the parent environment.
                        ##This line of code clears any value of i that had been cached by a 
                        ##prior execution of makeCacheMatrix()
        }
        
        get <- function() x                     
                        ##Defines the getter for the vector x
        
        setInv <- function(Inv) i <<- Inv    	
                        ##SetInv uses the <<- form of the assignment operator to assign   
                        ##the input argument to the value of i in the parent environment.
        
        getInv <- function() i                  
                        ##Defines the getter for the inverse m. R takes advantage of lexical
                        ##scoping to find the correct symbol i to retrieve its value.
        
        ##Getters and setters are now defined for both of the data objects within 
        ##the makeCacheMatrix() object.
        
        list(set = set, get = get,              
             setInv = setInv,
             getInv = getInv)
                        ##Assigns each of these functions as an element within a list(), 
                        ##and returns it to the parent environment
}

## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
##should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {                
        ##A single argument, x, and an ellipsis that allows the caller 
        ##to pass additional arguments into the function.
        
        i <- x$getInv()                         
                ##Calls the getInv() function on the input object.
        
        if(!is.null(i)) {                       
                        ##Checks to see whether the result is NULL
                        ##Since makeCacheMatrix() sets the cached inverse to NULL whenever 
                        ##a new vector is set into the object
                message("getting cached data")  
                        ##If not null (!is.null) then there is a valid cached inverse. 
                        ##In this case where something exists in the cache.print the 
                        ##message "getting cached data"
                return(i)                       
                        ##returns this cached inverse to the parent environment.  
                
        }
        data <- x$get()                         
                        ##Gets the vector from the input object
        i <- solve(data, ...)                 	
                        ##Solve(X) returns the inverse of a square invertible matrix 
        x$setInv(i)                             
                        ##Cet the inverse in the input object
        i                                       
                        ##Returns the value of the inverse to the parent environment by 
                        ##Printing the inverse object.
}


