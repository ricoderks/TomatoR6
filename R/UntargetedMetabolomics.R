#-------------------------------------------- Untargeted metabolomics class ----
#' @title R6 untargeted metabolomics class
#' 
#' @description
#' Untargeted metabolomics class. This is a child class from DataImport class.
#' 
#' @import R6
#' 
#' @export
#' 
#' @author Rico Derks
#' @author Yassene Mohammed
#'
UntargetedMetabolomics <- R6::R6Class(
  inherit = DataImport,
  classname = "UntargetedMetabolomics",
  public = list(
    #' @description
    #' Initialization function for DataImport class.
    #' 
    #' @param name character(1), name.
    #' @param filenames character(), containing the full file names.
    #' @param meta_filename character(1), containing the full meta file name.
    #' 
    initialize = function(name = NA, filenames = NA, meta_filename = NA) {
      super$initialize(name, filenames, meta_filename)
    }
    
    
    #---------------------------------------------------------- global info ----    
    
    
    #------------------------------------------------------- import methods ----
    
    
    #------------------------------------------ convert to standard methods ----
    # Rico: Methods to reshape / restructure the data to the standardized format
    # we specified.
    
    
    #----------------------------------------------------- plotting methods ----
    
    
  )
)