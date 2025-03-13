#' Defining the Sciex Multi Quant class
#'
#' @import R6
#'
#' @author Rico Derks
#' @author Yassene Mohammed
#' @author Marieke Heijink
#' 
#' @export
#' 
#' @name MultiQuant
NULL

MultiQuant <- R6::R6Class(
  inherit = DataImport,
  classname = "MultiQuant",
  active = list(
    file_curation = function(value) {
      if(missing(value)) {
        self$.file_curation
      } else {
        self$.file_curation <- value
        private$add_log(message = paste0("Added curation file: '", 
                                         self$.file_curation,
                                         "'"))
      }
    },
    regex_standards = function(value) {
      if(missing(value)) {
        private$.regex_standards
      } else {
        private$.regex_standards <- value
        private$add_log(message = paste0("Regular expression 'standards' set: '", 
                                         private$.regex_standards,
                                         "'"))
      }
    }
  ),
  public = list(
    initialize = function(name = NA) {
      super$initialize(name)
    },
    
    import = function() {
      cli::cli_h3("Importing")
      cli::cli_ul()
      
      # import meta data
      cli::cli_li("meta data")
      private$import_metadata()
      private$add_log(message = "Imported meta data.")
      
      # extracting indices
      cli::cli_li("extracting indices")
      private$extract_indices()
      private$add_log(message = "Extracted indices.")
      
      # curation data
      cli::cli_li("curation data")
      private$import_curation()
      private$add_log(message = "Imported curation data.")
      
      # import data
      cli::cli_li("experimental data")
      private$import_data()
      private$add_log(message = "Imported raw data.")
      
      cli::cli_end()
      cli::cli_alert_success("Done!")
    },
    
    #---------------------------------------------------------------- files ----
    .file_curation = NULL,
    
    #--------------------------------------------------------------- tables ----
    table_curation = NULL
  ),
  private = list(
    #---------------------------------------------------------------- print ----
    file_show = function() {
      utils_file_show_mq(self = self)
    },
    #-------------------------------------------------------------- indices ----
    .id_col_data = "Sample_Name",
    
    #----------------------------------------------------- parameters regex ----
    .regex_standards = NULL,
    
    #----------------------------------------------------- import functions ----
    import_data = function() {
      import_multiquant(self = self)
      private$extract_featuredata()
      # private$make_data_long()
      # private$make_data_wide()
    },
    import_curation = function() {
      import_curation_mq(self = self)
    },
    extract_featuredata = function() {
      extract_features_mq(self = self)
    }
  )
)