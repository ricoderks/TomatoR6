#' @title test function
#' 
#' @description
#' test function
#' 
#' @returns the number 3
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
#' @returns the sum of x and y
#' 
di_fun2 <- function(x, y) {
  return(x + y)
}

#' @title test function
#' 
#' @param self class object
#' 
#' @description
#' Can I show a public property?
#' 
#' @returns the content of the name property
#' 
di_fun3 <- function(self) {
  return(self$name)
}

#' @title test function
#' 
#' @param self class object
#' 
#' @description
#' Can I change a public property from here?
#' 
#' @returns self
#' 
di_fun4 <- function(self) {
  self$monkey <- "chimpanzee"
  invisible(self)
}