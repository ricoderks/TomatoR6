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
    data_to_extract = function(value) {
      if(missing(value)) {
        private$.data_to_extract
      } else {
        value <- match.arg(arg = value,
                           choices = c("Area", "Area_Ratio"))
        private$.data_to_extract <- value
        private$add_log(message = paste0("Data extracted: '", 
                                         private$.data_to_extract,
                                         "'"))
      }
    },
    extract_metadata = function(value) {
      if(missing(value)) {
        private$.extract_metadata
      } else {
        if(value) { 
          cli::cli_alert_info("Do **NOT** change the fields: ")
          cli::cli_li("`id_col_meta`")
          cli::cli_li("`order_column")
        }
        private$.extract_metadata <- value
        self$id_col_meta <- "sampleId"
        self$order_column <- "injOrder"
        private$add_log(message = paste0("Extract meta data from RAW data: '", 
                                         private$.extract_metadata,
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
  #----------------------------------------------------------------- PUBLIC ----
  public = list(
    initialize = function(name = NA) {
      super$initialize(name)
      # by default don't extract meta data from raw data.
      self$extract_metadata <- FALSE
    },
    
    import = function() {
      cli::cli_h3("Importing")
      cli::cli_ul()
      
      # import data
      cli::cli_li("experimental data")
      private$import_data()
      private$add_log(message = "Imported raw data.")
      
      # import meta data
      cli::cli_li("meta data")
      private$import_metadata()
      private$add_log(message = "Imported meta data.")
      
      if(self$extract_metadata) {
        cli::cli_li("extract meta data")
        private$extract_meta_from_raw()
        private$add_log(message = "Extracted meta data from raw data.")
      }
      
      # extracting indices
      cli::cli_li("extracting indices")
      private$extract_indices()
      private$add_log(message = "Extracted indices.")
      
      # curation data
      cli::cli_li("curation data")
      private$import_curation()
      private$add_log(message = "Imported curation data.")
      
      cli::cli_end()
      cli::cli_alert_success("Done!")
    },
    calc_qc = function() {
      cli::cli_h3("Calculate QC")
      cli::cli_li("calculating 'RSD'")
      private$calc_qcpool_rsd()
      private$add_log("Calculated RSD data qcpools!")
      
      cli::cli_li("calculating 'trend'")
      private$calc_qcpool_trend()
      private$add_log("Calculated trend data qcpools!")
      
      cli::cli_li("calculating 'correlation'")
      private$calc_correlation()
      private$add_log("Calculated sample/qcpools correlation!")
      
      cli::cli_li("calculating 'normalized area ratio'")
      private$calc_norm_arearatio()
      private$add_log("Calculated normalized area ratio!")
      cli::cli_alert_success("Done!")
    },
    
    #---------------------------------------------------------------- files ----
    .file_curation = NULL,
    
    #--------------------------------------------------------------- tables ----
    table_curation = NULL,
    table_norm_arearatio = NULL,
    table_norm_arearatio_long = NULL
  ),
  #---------------------------------------------------------------- PRIVATE ----
  private = list(
    #----------------------------------------------------------------- data ----
    .data_to_extract = NULL,
    .extract_metadata = NULL,
    
    #-------------------------------------------------------------- indices ----
    .id_col_data = "Sample_Name",
    
    #----------------------------------------------------- parameters regex ----
    .regex_standards = NULL,
    
    #---------------------------------------------------------------- print ----
    file_show = function() {
      utils_file_show_mq(self = self)
    },
    
    #----------------------------------------------------- import functions ----
    import_data = function() {
      import_multiquant(self = self)
      private$extract_featuredata()
      private$make_data_long()
      private$make_data_wide()
    },
    import_curation = function() {
      import_curation_mq(self = self)
      filter_curation_mq(self = self)
      # private$set_analysis_features()
      # private$extract_analysis_table()
    },
    extract_featuredata = function() {
      extract_features_mq(self = self)
    },
    extract_meta_from_raw = function() {
      extract_meta_from_raw(self = self)
    },
    make_data_long = function() {
      extract_table_long_mq(self = self)
    },
    make_data_wide = function() {
      make_table_wide(self = self)
    },
    calc_norm_arearatio = function() {
      qc_calc_norm_arearatio(self = self)
    }
  )
)