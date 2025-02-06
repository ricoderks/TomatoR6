#--------------------------------------------------------- DataImport class ----
#' @title R6 Data import class
#' 
#' @description
#' Data import class. This will be the parent class for several other classes.
#' 
#' @field name character(1), containing a name.
#' @field filenames character(), containing the full file names.
#' @field meta_filename character(1), containing the full meta file name.
#' @field tables list() containing all data tables.
#' @field indices list() containing all kind of indices.
#' @field params list(), containing all kind of parameter settings for the methods.
#' 
#' @import R6
#' 
#' @author Rico Derks
#' @author Yassene Mohammed
#'
DataImport <- R6::R6Class(
  classname = "DataImport",
  public = list(
    #' @description
    #' Initialization function for DataImport class.
    #' 
    #' @param name character(1), name.
    #' @param filenames character(), containing the full file names.
    #' @param meta_filename character(1), containing the full meta file name.
    #' 
    initialize = function(name = NA, filenames = NA, meta_filename = NA) {
      self$name <- name
      self$filenames <- filenames
      self$meta_filename <- meta_filename
    },
    #---------------------------------------------------------- global info ----
    name = NULL,
    filenames = NULL,
    meta_filename = NULL,
    
    
    #--------------------------------------------------------------- tables ----
    tables = list(
      meta_data = NULL,
      
      raw_data = NULL,
      
      # wide data
      all_data = NULL,
      blank_data = NULL,
      qc_data = NULL,
      pool_data = NULL,
      sample_data = NULL,
      
      # long data
      all_data_long = NULL,
      blank_data_long = NULL,
      qc_data_long = NULL,
      pool_data_long = NULL,
      sample_data_long = NULL,
      
      feature_data = NULL
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
    
    #------------------------------------------------------ general methods ----
    #' @description
    #' Print a nice summary of the class.
    #'
    #' @param ... not used at the moment.
    #'
    print = function(...) {
      cat(self$name, "contains:\n")
      cat("* data file(s):\t\t", paste(self$filenames, collapse = ", "), "\n")
      cat("* meta data file:\t", self$meta_filename, "\n")
    },
    
    
    #----------------------------------------------- generic import methods ----
    # Rico: I assume most import methods are platform specific and therefore 
    # probably the method will be in their respective child class, but generic
    # import methods can be placed here.
    #' @description
    #' This method imports all data and meta data.
    #' 
    import = function() {
      cat("Importing :\n")
      # import meta data
      cat("  * meta data\n")
      private$read_meta_data()
      # import data
      cat("  * experimental data\n")
      private$import_data()
      cat("Done!")
    },
    
    
    #-------------------------------------------- generic parameter methods ----
    #' @description
    #' Method to set all parameters
    #' 
    #' @param columns list(), column names containing information about, sample id,
    #' sample type, group column, batch column. Valid entries are: id_col_meta, 
    #' id_col_sample, type_column, group_column, batch_column, order_column.
    #' @param regex list(), regular expressions to recognize blanks, qcs, qcpools and samples.
    #' Valid entries are blanks, qcs, qcpools and samples.
    #' @param imputation list(), imputation parameters.
    #' @param batch_correction list(), batch correction parameters.
    #' @param blank_filtering list(), blank filtering parameters.
    #' @param normalization list(), normalization parameters.
    #' 
    #' @details Run this function first before importing the data.
    #' 
    set_parameters = function(columns = NULL,
                              regex = NULL,
                              imputation = NULL,
                              batch_correction = NULL,
                              blank_filtering = NULL,
                              normalization = NULL) {
      cat("Setting parameters:\n")
      if(!is.null(columns)) {
        private$set_parameters_columns(params = columns)
        cat("  * column names\n")
      }
      if(!is.null(regex)) {
        private$set_parameters_regex(params = regex)
        cat("  * regular expressions\n")
      }
      if(!is.null(imputation)) {
        # private$set_parameters_imputation(params = imputation)
        cat("  * imputation\n")
      }
      if(!is.null(batch_correction)) {
        # private$set_parameters_batch_correction(params = batch_correction)
        cat("  * batch correctoin\n")
      }
      if(!is.null(blank_filtering)) {
        # private$set_parameters_blank_filtering(params = blank_filtering)
        cat("  * blank filtering\n")
      }
      if(!is.null(normalization)) {
        # private$set_parameters_blank_normalization(params = normalization)
        cat("  * normalization\n")
      }
      
      cat("Done!\n")
    },
    
    
    
    
    #--------------------------------------------------- imputation methods ----
    
    
    #--------------------------------------------- batch correction methods ----
    #' @description Median batch correction
    #'
    #' @return data.frame with median batch corrected data.
    #'
    median_bc = function() {
      
    },
    
    #' @description Perform QC-RLSC for batch correction of chromatographic signal.
    #'
    #' @param tab table N*K (row * column) with N samples and K variables.
    #' @param colv vector N of numbers: 1 for QC samples and 2 for other samples.
    #' @param or vector of measuring order (see details).
    #' @param span the parameter alpha which controls the degree of smoothing.
    #' @param verbose print which variable has been corrected to monitor the process (default = FALSE).
    #'
    #' @return corrected table N*K
    #'
    #' @details Make sure that everything is sorted in measurement order!!!
    #'
    #' @importFrom stats loess approx
    #'
    #' @author E. Nevedomskaya
    #' @author Rico Derks
    #'
    #' @references Dunn et al. Nature Protocols 6, 1060-1083 (2011)
    #' 
    loess_bc = function(tab, colv, or, span = 0.75, verbose = FALSE) {
      tab_corr <- tab
      
      for (i in 1:ncol(tab)) {
        ll <- stats::loess(tab[which(colv == 1), i] ~ or[which(colv == 1)], span = span)
        
        aa <- stats::approx(x = or[which(colv == 1)],
                            y = ll$fitted,
                            xout = or)
        
        tab_corr[, i] <- tab[, i] / aa$y
        
        if(verbose == TRUE) {
          print(i)
        }
      }
      
      return(tab_corr)
    },
    
    #' @description
    #' Perform ComBat batch correction from the SVA package.
    #'
    #' @return data.frame with combat batch corrected data.
    #'
    combat_bc = function() {
      
    },
    
    
    #---------------------------------------------- blank filtering methods ----
    
    
    #------------------------------------------------ normalization methods ----
    
    
    #----------------------------------------------------- plotting methods ----
    #' @description
    #' A histogram showing the RSD distribution of all features.
    #' 
    #' @return ggplot2 object, containing a histogram.
    #' 
    plot_qc_rsd = function() {
      
    },
    
    #' @description
    #' Trend plot showing how each feature behaves in the pooled samples compared 
    #' to the first pooled sample in the batch.
    #' 
    #' @return ggplot2 object, containing a trend plot.
    #' 
    plot_qc_trend = function() {
      
    }
  ), # end public
  #---------------------------------------------------------------- private ----
  private = list(
    #---------------------------------------------------- meta data methods ----
    read_meta_data = function(file = NULL) {
      extension <- sub(pattern = ".*(csv|txt|xlsx)$",
                       replacement = "\\1",
                       x = self$meta_filename)
      
      meta_df <- switch(
        extension,
        "csv" = read.csv(file = self$meta_filename,
                         header = TRUE),
        "txt" = read.table(file = self$meta_filename,
                           header = TRUE,
                           sep = "\t"),
        "xlsx" = openxlsx::read.xlsx(xlsxFile = self$meta_filename)
      )
      
      self$tables$meta_data <- meta_df
    },
    
    #---------------------------------------------------- parameter methods ----
    set_parameters_columns = function(params = NULL) {
      if(!is.null(params$type_column)) {
        self$indices$type_column <- params$type_column
      }
      if(!is.null(params$group_column)) {
        self$indices$group_column <- params$group_column
      }
      if(!is.null(params$batch_column)) {
        self$indices$batch_column <- params$batch_column
      }
      if(!is.null(params$order_column)) {
        self$indices$order_column <- params$order_column
      }
    },
    
    set_parameters_regex = function(params = NULL) {
      if(!is.null(params$blanks)) {
        self$params$regex$blanks <- params$blanks
      }
      if(!is.null(params$qcs)) {
        self$params$regex$qcs <- params$qcs
      }
      if(!is.null(params$pools)) {
        self$params$regex$pools <- params$pools
      }
      if(!is.null(params$samples)) {
        self$params$regex$samples <- params$samples
      }
    },
    
    extract_indices = function() {
      # self$indices$index_blanks <- grep(pattern = self$params$regex$blanks,
      #                                   x = colnames())
    }
  )
)
