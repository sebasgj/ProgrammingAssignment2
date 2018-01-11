
# La inversa de una matriz suele ser un cálculo costoso y puede haber algún beneficio
# para almacenar en caché el inverso de una matriz en lugar de calcularlo repetidamente. las
# siguientes dos funciones se utilizan para almacenar en caché la inversa de una matriz.

# makeCacheMatrix crea una lista que contiene una función para
# 1. poner el valor de la matriz
# 2. obtener el valor de la matriz
# 3. poner el valor de inverso de la matriz
# 4. obtener el valor de inverso de la matriz.


makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinversa <- function(inversa) inv <<- inversa
    getinversa <- function() inv
    list(set=set, get=get, 
         setinversa = setinversa, 
         getinversa=getinversa)
}

cacheSolve <- function(x,...){
    inv <- x$getinversa()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinversa(inv)
    inv
}

##Ejemplo de aplicación 

x = rbind(c(1, -1/2), c(-1/2, 1))
matrix = makeCacheMatrix(x)
matrix$get()

##No hay valores para la cache en la primera ejecución
cacheSolve(matrix)

##Validamos que si este en cache la inversa
cacheSolve(matrix)
