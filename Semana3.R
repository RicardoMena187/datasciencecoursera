##Creamos una funcion con una matriz

makeCacheMatrix <- function(x  =  matrix ()) {
  r <- NULL
  set <- function(y) {
    x <<- y
    r <<- NULL
  }
  get <- function() x
  setresol <- function(resol) r <<- resol
  getresol <- function() r
  list(set = set, get = get,
       setresol = setresol,
       getresol = getresol)
}

##La funcion que vamos a guardar en cache es el determinante de la misma
cacheResol <- function(x, ...) {
  r <- x$getresol()
  if(!is.null(r)) {
    message("Obteniendo cache matriz")
    return(r)
  }
  data <- x$get()
  r <- solve(data, ...)
  x$setresol(r)
  r
}
