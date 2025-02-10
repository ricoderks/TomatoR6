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
    #' 
    initialize = function(name = NA) {
      super$initialize(name)
      
      private$add_log(message = "Created untargeted metabolomics object")
      
      cli::cli_h2("Untargeted metabolomics")
      cli::cli_text("Untarageted metabolomics object created.")
      cli::cli_rule()
      cli::cli_text("Please set the following with `set_files()`:")
      files <- cli::cli_ul()
      cli::cli_li("Data file names")
      cli::cli_li("Meta data files")
      cli::cli_end(files)
      cli::cli_rule()
      cli::cli_text("Please set the following with `set_parameters()`:")
      params <- cli::cli_ul()
      cli::cli_li("ID columns for data and meta data")
      cli::cli_li("Column names, i.e. sample type, group, etc.")
      cli::cli_li("Regular expression, to recognize the diifferent samples.")
      cli::cli_end(params)
      cli::cli_rule()
      cli::cli_text("Next step: run `import()` to import all data.")
    }
    
    
    #---------------------------------------------------------- global info ----    
    
    
    #------------------------------------------------------- import methods ----
    
    
    #------------------------------------------ convert to standard methods ----
    # Rico: Methods to reshape / restructure the data to the standardized format
    # we specified.
    
    
    #----------------------------------------------------- plotting methods ----
    
    
  )
)