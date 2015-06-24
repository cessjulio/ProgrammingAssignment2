## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        
      stor <- NULL # Para almacenar resultado      
      set <- function(y) {
	  x <<- y
	  stor <<- NULL # Inicializar variable en NULL
      }

      get <- function() x # retornar valores la matrix
      setInv <- function(inv) stor <<- inv # colocar la matrix inversa
      getInv <- function() stor # retornar la matrix matrix     
      list(set = set, get = get,
	       setInv = setInv,
	       getInv = getInv)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInv() # Obtener el valor de la matrix en X
      if(!is.null(m)) {  # Validar si es null
	message("Obteniendo cached data")
	return(m)  #retonar el calculo
      }
      data <- x$get() 
      m <- solve(data)
      x$setInv(m) 
      m 
}
