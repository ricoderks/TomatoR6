#' Defining the DataImport class
#'
#' @export
#' 
#' @name DataImport
NULL

DataImport <- R6::R6Class(
  classname = "DataImport",
  public = list(
    initialize = function(name = NA) {
      self$name <- name
    },
    #---------------------------------------------------------- global info ----
    name = NULL,
    monkey = NULL,
    
    #------------------------------------------------------- some functions ----
    fun1 = function() {
      di_fun1()
    },
    fun2 = function(x, y) {
      di_fun2(x, y)
    },
    fun3 = function() {
      di_fun3(self)
    },
    fun4 = function() {
      di_fun4(self)
    }
  )
)


