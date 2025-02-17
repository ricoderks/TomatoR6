#' Defining the Untargeted metabolomics class
#'
#' @import R6
#'
#' @author Rico Derks
#' @author Yassene Mohammed
#' 
#' @export
#' 
#' @name UntargetedMetabolomics
NULL

UntargetedMetabolomics <- R6::R6Class(
  inherit = DataImport,
  classname = "UntargetedMetabolomics",
  #----------------------------------------------------------------- public ----
  public = list(
    initialize = function(name = NA) {
      super$initialize(name)
    }
    
    
    #---------------------------------------------------------- global info ----    
    
    
    #------------------------------------------------------- import methods ----
    
    
    #------------------------------------------ convert to standard methods ----
    # Rico: Methods to reshape / restructure the data to the standardized format
    # we specified.
    
    
    #----------------------------------------------------- plotting methods ----
    
    
  )
)