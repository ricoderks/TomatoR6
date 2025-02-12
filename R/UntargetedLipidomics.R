#---------------------------------------------- Untargeted lipidomics class ----
#' @title R6 untargeted lipidomics class
#' 
#' @description
#' Untargeted lipidomics class. This is a child class from DataImport class.
#' 
#' @import R6
#' 
#' @export
#' 
#' @author Rico Derks
#' @author Yassene Mohammed
#'
UntargetedLipidomics <- R6::R6Class(
  inherit = DataImport,
  classname = "UntargetedLipidomics",
  public = list(
    #' @description
    #' Initialization function for DataImport class.
    #' 
    #' @param name character(1), name.
    #' 
    initialize = function(name = NA) {
      super$initialize(name)
    }
  )
)