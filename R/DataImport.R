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
      blank_data = NULL,
      qc_data = NULL,
      pool_data = NULL,
      sample_data = NULL,
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
      id_col_data = NULL,
      id_col_feature = NULL,
      
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
    
    
    #---------------------------------------------------- meta data methods ----
    #' @description
    #' Read all kind of meta data files (.csv, .txt, .xlsx).
    #' 
    #' @param file character(1), containing the full path to the meta data file.
    #' 
    #' @return data.frame containing the meta data.
    #' 
    read_meta_data = function(file = NULL) {
      
    },
    
    
    #----------------------------------------------------- pivoting methods ----
    #' @description
    #' Function to pivot a data.frame into long (tidy) format.
    #' 
    #' @return data.frame in long format.
    #' 
    make_table_long = function() {
      
    },
    
    #' @description
    #' Function to pivot a data.frame into wide format.
    #' 
    #' @return data.frame in wide format.
    #'
    make_table_wide = function() {
      
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
  )
)
