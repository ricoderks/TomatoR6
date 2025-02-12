#' @title test function
#' 
#' @description
#' test function
#' 
di_fun1 <- function() {
  return(1 + 2)
}

#' @title test function
#' 
#' @description
#' test function
#' 
#' @param x numeric()
#' @param y numeric()
#' 
di_fun2 <- function(x, y) {
  return(x + y)
}

#' @title test function
#' 
#' @description
#' test function
#' 
di_fun3 <- function(self) {
  return(self$name)
}