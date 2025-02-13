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
    table_metadata = function(value) {
      if(missing(value)) {
        self$.table_metadata
      } else {
        self$.table_metadata <- value
      }
    },
    table_rawdata = function(value) {
      if(missing(value)) {
        self$.table_rawdata
      } else {
        self$.table_rawdata <- value
      }
    },
    table_alldata = function(value) {
      if(missing(value)) {
        self$.table_alldata
      } else {
        self$.table_alldata <- value
      }
    },
    table_blank = function(value) {
      if(missing(value)) {
        self$.table_blank
      } else {
        self$.table_blank <- value
      }
    },
    table_qc = function(value) {
      if(missing(value)) {
        self$.table_qc
      } else {
        self$.table_qc <- value
      }
    },
    table_pool = function(value) {
      if(missing(value)) {
        self$.table_pool
      } else {
        self$.table_pool <- value
      }
    },
    table_sample = function(value) {
      if(missing(value)) {
        self$.table_sample
      } else {
        self$.table_sample <- value
      }
    },
    table_alldata_long = function(value) {
      if(missing(value)) {
        self$.table_alldata_long
      } else {
        self$.table_alldata_long <- value
      }
    },
    table_blank_long = function(value) {
      if(missing(value)) {
        self$.table_blank_long
      } else {
        self$.table_blank_long <- value
      }
    },
    table_qc_long = function(value) {
      if(missing(value)) {
        self$.table_qc_long
      } else {
        self$.table_qc_long <- value
      }
    },
    table_pool_long = function(value) {
      if(missing(value)) {
        self$.table_pool_long
      } else {
        self$.table_pool_long <- value
      }
    },
    table_sample_long = function(value) {
      if(missing(value)) {
        self$.table_sample_long
      } else {
        self$.table_sample_long <- value
      }
    }
  ),
  public = list(
    initialize = function(name = NA) {
      self$name <- name
    },
    #---------------------------------------------------------- global info ----
    name = NULL,
    
    #---------------------------------------------------------------- files ----
    .file_data = NULL,
    .file_meta = NULL,
    
    #--------------------------------------------------------------- tables ----
    .table_metadata = NULL,
    .table_rawdata = NULL,
    
    # wide data
    .table_alldata = NULL,
    .table_blank = NULL,
    .table_qc = NULL,
    .table_pool = NULL,
    .table_sample = NULL,
    
    # long data
    .table_alldata_long = NULL,
    .table_blank_long = NULL,
    .table_qc_long = NULL,
    .table_pool_long = NULL,
    .table_sample_long = NULL,
    
    #--------------------------------------------------------------- tables ----
    tables = list(
      
      feature_data = NULL,
      
      plot_rsd_data = NULL,
      plot_trend_data = NULL
      # Rico: What about the rt, m/z tables? Assuming that above tables contain
      # peak areas.
      # Rico: Do we also want to store normalized tables, batch corrected tables?
      # Or do we keep track of which preprocessing we do and only store the end 
      # result?
    ),
    
    
    #-------------------------------------------------------------- indices ----
    indices = list(
      id_col_meta = NULL,
      id_col_data = "sampleName",
      id_col_feature = "id",
      
      type_column = NULL,
      group_column = NULL,
      batch_column = NULL,
      order_column = NULL,
      
      index_blanks = NULL,
      index_pools = NULL,
      index_qcs = NULL,
      index_samples = NULL
    ),
    
    
    #----------------------------------------------------------- parameters ----
    params = list(
      regex = list(
        blanks = NULL,
        qcs = NULL,
        pools = NULL,
        samples = NULL
      ),
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
      # private$samples_fun()
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

      # # extracting indices
      # cli::cli_li("extracting indices")
      # private$add_log(message = "Extracting indices")
      # private$extract_indices()
      # 
      # # extracting tables
      # cli::cli_li("extracting tables")
      # private$add_log(message = "Extracting tables")
      # private$extract_tables()
      
      cli::cli_alert_success("Done!")
    }
    # fun1 = function() {
    #   di_fun1()
    # },
    
  ), # end public
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
    import_metadata = function() {
      import_read_metadata(self = self)
    }
  )
)


