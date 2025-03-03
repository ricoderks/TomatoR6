#' Defining the DataImport class
#'
#' @import R6
#' @import cli
#' @importFrom stats median
#'
#' @author Rico Derks
#' @author Yassene Mohammed
#' 
#' @name DataImport
NULL

DataImport <- R6::R6Class(
  classname = "DataImport",
  #-------------------------------------------------------- ACTIVE BINDINGS ----
  active = list(
    file_data = function(value) {
      if(missing(value)) {
        self$.file_data
      } else {
        self$.file_data <- value
        private$add_log(message = paste0("Added data files: ", 
                                         paste(basename(self$.file_data), collapse = ", ")))
      }
    },
    file_meta = function(value) {
      if(missing(value)) {
        self$.file_meta
      } else {
        self$.file_meta <- value
        private$add_log(message = paste0("Added data files: ", 
                                         paste(basename(self$.file_meta), collapse = ", ")))
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
    preprocessing_steps = function(value) {
      if(missing(value)) {
        self$.preprocessing_steps
      } else {
        if(all(value %in% private$steps_preprocessing)) {
          self$.preprocessing_steps <- value
          private$add_log(message = paste0("Pre-processing steps: ", 
                                           paste(paste0("'", self$.preprocessing_steps, "'"),
                                                 collapse = ", ")))
        } else {
          cli::cli_alert_danger("An unknown pre-processing step is specified!")
          cli::cli_alert_info("Valid pre-processing steps are:")
          steps <- cli::cli_ul()
          for(a in private$steps_preprocessing) {
            cli::cli_li(paste0("'", a, "'"))
          }
          cli::cli_end(steps)
        }
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
    },
    qc_rsd_limit = function(value) {
      if(missing(value)) {
        self$.qc_rsd_limit
      } else {
        self$.qc_rsd_limit <- value
        private$add_log(message = paste0("QC RSD limit set: ", 
                                         self$.qc_rsd_limit))
      }
    },
    blank_ratio = function(value) {
      if(missing(value)) {
        self$.blank_ratio
      } else {
        self$.blank_ratio <- value
        private$add_log(message = paste0("Sample / blank ratio set: ", 
                                         self$.blank_ratio))
      }
    },
    blank_threshold = function(value) {
      if(missing(value)) {
        self$.blank_threshold
      } else {
        self$.blank_threshold <- value
        private$add_log(message = paste0("Sample threshold set: ", 
                                         self$.blank_threshold))
      }
    },
    blank_group_threshold = function(value) {
      if(missing(value)) {
        self$.blank_group_threshold
      } else {
        self$.blank_group_threshold <- value
        private$add_log(message = paste0("Group threshold set: ", 
                                         self$.blank_group_threshold))
      }
    },
    norm_pqn_reference = function(value) {
      if(missing(value)) {
        self$.norm_pqn_reference
      } else {
        value <- match.arg(arg = value,
                           choices = c("median", "mean"))
        self$.norm_pqn_reference <- value
        private$add_log(message = paste0("PQN normalisation reference set: ", 
                                         self$.norm_pqn_reference))
      }
    },
    imp_method = function(value) {
      if(missing(value)) {
        self$.imp_method
      } else {
        value <- match.arg(arg = value,
                           choices = names(private$impute_methods))
        self$.imp_method <- value
        private$add_log(message = paste0("Imputation method set: ", 
                                         self$.imp_method))
      }
    },
    bc_method = function(value) {
      if(missing(value)) {
        self$.bc_method
      } else {
        value <- match.arg(arg = value,
                           choices = private$bc_methods)
        self$.bc_method <- value
        private$add_log(message = paste0("Batch correction method set: ", 
                                         self$.bc_method))
      }
    },
    bc_loess_method = function(value) {
      if(missing(value)) {
        self$.bc_loess_method
      } else {
        self$.bc_loess_method <- value
        private$add_log(message = paste0("Batch correction loess method set: ", 
                                         self$.bc_loess_method))
      }
    },
    bc_loess_span = function(value) {
      if(missing(value)) {
        self$.bc_loess_span
      } else {
        self$.bc_loess_span <- value
        private$add_log(message = paste0("Batch correction loess span set: ", 
                                         self$.bc_loess_span))
      }
    }
    
  ), # end active bindings
  #----------------------------------------------------------------- PUBLIC ----
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
    table_analysis = NULL,
    
    # long data
    table_alldata_long = NULL,
    table_analysis_long = NULL,
    
    table_featuredata = NULL,
    
    table_rsd_data = NULL,
    table_trend_data = NULL,
    table_cor_data = NULL,
    table_blank_filtering = NULL,
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
    
    #-------------------------------------------------------- parameters QC ----
    .qc_rsd_limit = NULL, # 0.3
    
    #--------------------------------------------- parameter pre-processing ----
    .preprocessing_steps = NULL,
    
    #------------------------------------------- parameters blank filtering ----
    .blank_ratio = NULL, # 5
    .blank_threshold = NULL, # 0.8
    .blank_group_threshold = NULL, # 0.8
    
    #--------------------------------------------- parameters normalisation ----
    .norm_pqn_reference = NULL,
    
    #------------------------------------------------ parameters imputation ----
    .imp_method = NULL,
    
    #------------------------------------------ parameters batch correction ----
    .bc_method = NULL,
    .bc_loess_method = NULL,
    .bc_loess_span = NULL,
    
    #-------------------------------------------------------------- history ----
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
      
      # extracting indices
      cli::cli_li("extracting indices")
      private$extract_indices()
      private$add_log(message = "Extracted indices.")
      
      # import data
      cli::cli_li("experimental data")
      private$import_data()
      private$add_log(message = "Imported raw data.")
      
      cli::cli_alert_success("Done!")
    },
    preprocessing = function() {
      cli::cli_h3("Pre-processing steps")
      steps <- cli::cli_ul()
      
      private$reset_tables()
      
      for(a in self$preprocessing_steps) {
        cli::cli_li(paste0("applying: '", a, "'"))
        switch(
          a,
          "rsd_filter" = private$step_rsd(),
          "blank_filter" = private$step_blank_filter(),
          "total_normalisation" = private$step_total_normalisation(),
          "pqn_normalisation" = private$step_pqn_normalisation(),
          "imputation" = private$step_imputation(),
          "batch_correction" = private$step_batchcorrection(),
          NULL
        )
      }
      
      cli::cli_end(steps)
      cli::cli_alert_success("Done!")
    },
    
    #--------------------------------------------------------- qc functions ----
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
      
      cli::cli_alert_success("Done!")
    },
    plot_qc_rsd = function(type = NULL) {
      qc_plot_rsd(self = self,
                  type = type)
    },
    plot_qc_trend = function(type = NULL) {
      qc_plot_trend(self = self,
                    type = type)
    },
    plot_qc_cor = function(type = NULL) {
      qc_plot_cor(self = self)
    }
    
  ), # end public
  #---------------------------------------------------------------- PRIVATE ----
  private = list(
    #------------------------------------------------------- some functions ----
    add_log = function(message = NULL) {
      utils_add_log(self = self,
                    message = message)
    },
    #---------------------------------------------------------------- print ----
    file_show = function() {
      utils_file_show(self = self)
    },
    samples_show = function() {
      utils_sample_show(self = self)
    },
    #--------------------------------------------------------------- import ----
    import_metadata = function() {
      import_read_metadata(self = self)
    },
    extract_indices = function() {
      extract_indices(self = self)
    },
    #------------------------------------------------------------------- qc ----
    calc_qcpool_rsd = function() {
      qc_calc_rsd(self = self,
                  private = private)
    },
    calc_qcpool_trend = function() {
      qc_calc_trend(self = self)
    },
    calc_correlation = function() {
      qc_calc_cor(self = self)
    },
    #------------------------------------------------------ blank filtering ----
    calc_sample_blank_ratio = function() {
      blank_calc_ratio(self = self)
    },
    #----------------------------------------------------------- imputation ----
    impute_methods = list(
      "max" = max,
      "mean" = mean,
      "median" = stats::median,
      "min" = min
    ),
    #----------------------------------------------------- batch correction ----
    bc_methods = c(
      "median",
      "loess",
      "combat"
    ),
    #------------------------------------------------- pre-processing steps ----
    steps_preprocessing = c(
      "rsd_filter",
      "blank_filter",
      "total_normalisation",
      "pqn_normalisation",
      "imputation",
      "batch_correction"
    ),
    reset_tables = function() {
      utils_reset_tables(self = self)
    },
    step_rsd = function() {
      private$calc_qcpool_rsd()
      private$apply_rsd_filter()
      private$set_analysis_features()
      private$extract_analysis_table()
      private$add_log("Applied RSD filtering.")
    },
    step_blank_filter = function() {
      private$calc_sample_blank_ratio()
      private$apply_blank_filter()
      private$set_analysis_features()
      private$extract_analysis_table()
      private$add_log("Applied blank filtering.")
    },
    step_total_normalisation = function() {
      norm_total_area(self = self)
      private$add_log("Applied total area normalisation.")
    },
    step_pqn_normalisation = function() {
      norm_total_area(self = self)
      norm_pqn(self = self)
      private$add_log("Applied PQN normalisation.")
    },
    step_imputation = function() {
      impute_na(self = self,
                private = private)
      private$add_log("Applied imputation.")
    },
    step_batchcorrection = function() {
      batchcorrection(self = self)
      private$add_log("Applied batch correction.")
    },
    apply_rsd_filter = function() {
      qc_apply_rsd(self = self)
    },
    apply_blank_filter = function() {
      blank_apply_filter(self = self)
    },
    set_analysis_features = function() {
      utils_analysis_features(self = self)
    },
    extract_analysis_table = function() {
      utils_analysis_table(self = self)
    }
  )
)


