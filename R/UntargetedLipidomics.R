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
    #' @param filenames character(), containing the full file names.
    #' @param meta_filename character(1), containing the full meta file name.
    #' 
    initialize = function(name = NA, filenames = NA, meta_filename = NA) {
      super$initialize(name, filenames, meta_filename)
    },
    
    
    #---------------------------------------------------------- global info ----    
    
    
    #------------------------------------------------------- import methods ----
    
    
    #------------------------------------------ convert to standard methods ----
    # Rico: Methods to reshape / restructure the data to the standardized format
    # we specified.
    
    
    #----------------------------------------------------- plotting methods ----
    #' @description
    #' A violin plot showing the RSD of each lipid species per lipid class.
    #' 
    #' @return ggplot2 object containing a violin plot.
    #' 
    plot_qc_rsd_class = function() {
      # Rico: This might also useful for the untargeted metabolomics. MSDIAL 
      # also gives a class (ontology) for identified metabolites if available.
      # Maybe even for other child classes as well. Needs to be place in parent 
      # class then.
    }
  )
)