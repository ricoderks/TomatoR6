#--------------------------------------------------------- DataImport class ----
#' @title R6 Data import class
#' 
#' @description
#' Data import class. This will be the parent class for several other classes.
#' 
#' @field name character(1), containing a name.
#' @field files list(), containing all files.
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
    #' 
    initialize = function(name = NA) {
      self$name <- name
    },
    #---------------------------------------------------------- global info ----
    name = NULL,
    
    
    #---------------------------------------------------------------- files ----
    files = list(
      data_files = NULL,
      meta_filename = NULL  
    ),
    
    
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
      cat(self$name, ":\n")
      cat("* Files:\n")
      cat("\t* Data file(s):\t\t", paste(self$files$data_files, collapse = ", "), "\n")
      cat("\t* Meta data file:\t", self$files$meta_filename, "\n")
      cat("* Sample type:\n")
      cat("\t* Number of blanks:\t", length(self$indices$index_blanks), "\n")
      cat("\t* Number of qcs:\t", length(self$indices$index_qcs), "\n")
      cat("\t* Number of pools:\t", length(self$indices$index_pools), "\n")
      cat("\t* Number of samples:\t", length(self$indices$index_samples), "\n")
    },
    
    
    #--------------------------------------------------------- file methods ----
    #' @description
    #' Set the file names for data and meta data.
    #' 
    #' @param data_files character(), containing the full file names.
    #' @param meta_file character(1), containing the full meta file name.
    #' 
    set_files = function(data_files = NULL, meta_file = NULL) {
      cat("Setting files:\n")
      if(!is.null(data_files)) {
        cat("  * data files\n")
        self$files$data_files <- data_files
      }
      if(!is.null(meta_file)) {
        cat("  * meta data file\n")
        self$files$meta_filename <- meta_file
      }
      cat("Done!")
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
      
      # extracting indices
      cat("  * extracting indices\n")
      private$extract_indices()
      
      # extracting tables
      cat("  * extracting tables\n")
      private$extract_tables()
      
      cat("Done!")
    },
    
    
    #-------------------------------------------- generic parameter methods ----
    #' @description
    #' Method to set all parameters
    #' 
    #' @param ids list(), column names containing information about, sample id,
    #' feature id. Valid entries are: id_col_meta, id_col_sample.
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
    set_parameters = function(ids = NULL,
                              columns = NULL,
                              regex = NULL,
                              imputation = NULL,
                              batch_correction = NULL,
                              blank_filtering = NULL,
                              normalization = NULL) {
      cat("Setting parameters:\n")
      if(!is.null(ids)) {
        private$set_parameters_ids(params = ids)
        cat("  * id's\n")
      }
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
                       x = self$files$meta_file)
      
      meta_df <- switch(
        extension,
        "csv" = read.csv(file = self$files$meta_file,
                         header = TRUE),
        "txt" = read.table(file = self$files$meta_file,
                           header = TRUE,
                           sep = "\t"),
        "xlsx" = openxlsx::read.xlsx(xlsxFile = self$files$meta_file)
      )
      
      self$tables$meta_data <- meta_df
    },
    
    #---------------------------------------------------- parameter methods ----
    set_parameters_ids = function(params = NULL) {
      if(!is.null(params$id_col_meta)) {
        self$indices$id_col_meta <- params$id_col_meta
      }
      if(!is.null(params$id_col_data)) {
        self$indices$id_col_data <- params$id_col_data
      }
    },
    
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
      if(!is.null(self$tables$meta_data) & !is.null(self$indices$id_col_meta)) {
        if(!is.null(self$params$regex$blanks)) {
          self$indices$index_blanks <- 
            self$tables$meta_data[grep(pattern = self$params$regex$blanks,
                                       x = self$tables$meta_data[, self$indices$id_col_meta],
                                       ignore.case = TRUE), self$indices$id_col_meta]
        }
        
        if(!is.null(self$params$regex$qcs)) {
          self$indices$index_qcs <- 
            self$tables$meta_data[grep(pattern = self$params$regex$qcs,
                                       x = self$tables$meta_data[, self$indices$id_col_meta],
                                       ignore.case = TRUE), self$indices$id_col_meta]
        }
        
        if(!is.null(self$params$regex$pools)) {
          self$indices$index_pools <- 
            self$tables$meta_data[grep(pattern = self$params$regex$pools,
                                       x = self$tables$meta_data[, self$indices$id_col_meta],
                                       ignore.case = TRUE), self$indices$id_col_meta]
        }
        
        if(!is.null(self$params$regex$samples)) {
          self$indices$index_samples <- 
            self$tables$meta_data[grep(pattern = self$params$regex$samples,
                                       x = self$tables$meta_data[, self$indices$id_col_meta],
                                       ignore.case = TRUE), self$indices$id_col_meta]
        }
      }
    },
    
    extract_tables = function() {
      if(!is.null(self$indices$index_blanks)) {
        self$tables$blank_data <- 
          self$tables$all_data[self$tables$all_data[, self$indices$id_col_data, drop = TRUE] %in% self$indices$index_blanks, ]
        self$tables$blank_data_long <- 
          self$tables$all_data_long[self$tables$all_data_long[, self$indices$id_col_data, drop = TRUE] %in% self$indices$index_blanks, ]
      }
      
      if(!is.null(self$indices$index_qcs)) {
        self$tables$qc_data <- 
          self$tables$all_data[self$tables$all_data[, self$indices$id_col_data, drop = TRUE] %in% self$indices$index_qcs, ]
        self$tables$qc_data_long <- 
          self$tables$all_data_long[self$tables$all_data_long[, self$indices$id_col_data, drop = TRUE] %in% self$indices$index_qcs, ]
      }
      
      if(!is.null(self$indices$index_pools)) {
        self$tables$pool_data <- 
          self$tables$all_data[self$tables$all_data[, self$indices$id_col_data, drop = TRUE] %in% self$indices$index_pools, ]
        self$tables$pool_data_long <- 
          self$tables$all_data_long[self$tables$all_data_long[, self$indices$id_col_data, drop = TRUE] %in% self$indices$index_pools, ]
      }
      
      if(!is.null(self$indices$index_samples)) {
        self$tables$sample_data <- 
          self$tables$all_data[self$tables$all_data[, self$indices$id_col_data, drop = TRUE] %in% self$indices$index_samples, ]
        self$tables$sample_data_long <- 
          self$tables$all_data_long[self$tables$all_data_long[, self$indices$id_col_data, drop = TRUE] %in% self$indices$index_samples, ]
      }
    }
  )
)
