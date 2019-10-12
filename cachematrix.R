## Put comments here that give an overall description of what your
## functions do


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  ## Запишем матрицу
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  ## Вызовем матрицу
  get <- function() x
  ## Метод записи обратной матрицы
  setInverse <- function(solveMatrix) inv <<- solveMatrix
  ## Способ вызова обратной матрицы
  getInverse <- function() inv
  ## Отобразим список методов
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


cacheSolve <- function(x, ...) {
  ## Возвращает матрицу, обратную x
  inv <- x$getInverse()
  ## Убираем инверсию если она есть
  if(!is.null(inv)){
    message("Getting cached data.")
    return(inv)
  }
  ## Вызовем матрицу
  data <- x$get()
  ## Используем матричное умножение
  inv <- solve(data)
  x$setInverse(inv)
  inv      
}