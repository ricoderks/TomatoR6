#' Defining the DataImport class
#'
#' @import R6
#' @import cli
#'
#' @author Rico Derks
#' @author Yassene Mohammed
#' 
#' @name DataImport
NULL

DataImport <- R6::R6Class(
  classname = "DataImport",
  public = list(
    initialize = function(name = NA) {
      self$name <- name
      private$add_log(paste0("Created object: ", class(self)[1]))
    },
    #---------------------------------------------------------- global info ----
    name = NULL,
    
    #---------------------------------------------------------------- files ----
    .file_data = NULL,
    .file_meta = NULL,
    
    #--------------------------------------------------------------- tables ----
    table_metadata = NULL,
    table_rawdata = NULL,
    
    # wide data
    table_alldata = NULL,
    table_blank = NULL,
    table_qc = NULL,
    table_pool = NULL,
    table_sample = NULL,
    
    # long data
    table_alldata_long = NULL,
    table_blank_long = NULL,
    table_qc_long = NULL,
    table_pool_long = NULL,
    table_sample_long = NULL,
    
    table_featuredata = NULL,
    
    table_rsd_data = NULL,
    table_trend_data = NULL,
    #-------------------------------------------------------------- indices ----
    .id_col_meta = NULL,
    id_col_data = "sampleName",
    id_col_feature = "id",
    
    type_column = NULL,
    group_column = NULL,
    batch_column = NULL,
    order_column = NULL,
    
    index_blanks = NULL,
    index_pools = NULL,
    index_qcs = NULL,
    index_samples = NULL,
    
    #----------------------------------------------------- parameters regex ----
    .regex_blanks = NULL,
    .regex_qcs = NULL,
    .regex_pools = NULL,
    .regex_samples = NULL,
    
    #----------------------------------------------------------- parameters ----
    params = list(
      rsd = list(
        rsd_limit = NULL
      ),
      imputation = list(
        method = NULL
      ),
      batch_correction = list(
        method = NULL
      ),
      blank_filtering = list(
        sample_blank_ratio = 5,
        sample_threshold = 0.8,
        group_threshold = 0.8
      ),
      normalization = list(
        method = NULL
      )
    ),
    
    history = data.frame(id = NULL,
                         datetime = NULL,
                         message = NULL),
    
    
    #------------------------------------------------------ generic methods ----
    print = function(...) {
      cli::cli_h2(self$name)
      cli::cli_h3("Files")
      private$file_show()
      cli::cli_h3("Sample types")
      private$samples_show()
    },
    
    import = function() {
      cli::cli_h3("Importing")
      cli::cli_ul()
      
      # import meta data
      cli::cli_li("meta data")
      private$import_metadata()
      private$add_log(message = "Imported meta data.")
      
      
      # import data
      cli::cli_li("experimental data")
      private$import_data()
      private$add_log(message = "Imported raw data.")

      # extracting indices
      cli::cli_li("extracting indices")
      private$extract_indices()
      private$add_log(message = "Extracted indices.")
      
      # extracting tables
      cli::cli_li("extracting tables")
      private$extract_tables()
      private$add_log(message = "Extracted tables.")
      
      cli::cli_alert_success("Done!")
    }
    
  ), # end public
  #-------------------------------------------------------- active bindings ----
  active = list(
    file_data = function(value) {
      if(missing(value)) {
        self$.file_data
      } else {
        self$.file_data <- value
        private$add_log(message = paste0("Added data files: ", 
                                         paste(self$.file_data, collapse = ", ")))
      }
    },
    file_meta = function(value) {
      if(missing(value)) {
        self$.file_meta
      } else {
        self$.file_meta <- value
        private$add_log(message = paste0("Added data files: ", 
                                         paste(self$.file_meta, collapse = ", ")))
      }
    },
    id_col_meta = function(value) {
      if(missing(value)) {
        self$.id_col_meta
      } else {
        self$.id_col_meta <- value
        private$add_log(message = paste0("Set column ID meta data: '", 
                                         self$.id_col_meta,
                                         "'"))
      }
    },
    regex_blanks = function(value) {
      if(missing(value)) {
        self$.regex_blanks
      } else {
        self$.regex_blanks <- value
        private$add_log(message = paste0("Regular expression blank set: '", 
                                         self$.regex_blanks,
                                         "'"))
      }
    },
    regex_qcs = function(value) {
      if(missing(value)) {
        self$.regex_qcs
      } else {
        self$.regex_qcs <- value
        private$add_log(message = paste0("Regular expression qcs set: '", 
                                         self$.regex_qcs,
                                         '"'))
      }
    },
    regex_pools = function(value) {
      if(missing(value)) {
        self$.regex_pools
      } else {
        self$.regex_pools <- value
        private$add_log(message = paste0("Regular expression pools set: '", 
                                         self$.regex_pools,
                                         "'"))
      }
    },
    regex_samples = function(value) {
      if(missing(value)) {
        self$.regex_samples
      } else {
        self$.regex_samples <- value
        private$add_log(message = paste0("Regular expression samples set: '", 
                                         self$.regex_samples,
                                         "'"))
      }
    }
    
  ),
  #---------------------------------------------------------------- private ----
  private = list(
    #------------------------------------------------------- some functions ----
    add_log = function(message = NULL) {
      utils_add_log(self = self,
                    message = message)
    },
    file_show = function() {
      utils_file_show(self = self)
    },
    samples_show = function() {
      utils_sample_show(self = self)
    },
    import_metadata = function() {
      import_read_metadata(self = self)
    },
    extract_indices = function() {
      extract_indices(self = self)
    },
    extract_tables = function() {
      extract_tables(self = self)
    }
  )
)


