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
#' @field history data.frame() with the history of all steps.
#' 
#' @import R6
#' @import cli
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
    #' @description
    #' Print a nice summary of the class.
    #'
    #' @param ... not used at the moment.
    #'
    print = function(...) {
      cli::cli_h2(self$name)
      cli::cli_h3("Files")
      private$files_fun()
      cli::cli_h3("Sample types")
      private$samples_fun()
      
    },
    
    
    #' @description
    #' Show information about the data.
    #' 
    show_data_info = function() {
      cli::cli_h2("Data")
      cli::cli_h3("Features")
      cli::cli_ul(paste("Number of features:", nrow(self$tables$feature_data)))
      cli::cli_h3("Sample types")
      cli::cli_ul()
      cli::cli_li(paste("NUmber of blanks:", length(self$indices$index_blanks)))
      cli::cli_li(paste("NUmber of qcs:", length(self$indices$index_qcs)))
      cli::cli_li(paste("NUmber of pools:", length(self$indices$index_pools)))
      cli::cli_li(paste("NUmber of samples:", length(self$indices$index_samples)))
      cli::cli_end()
    },
    
    
    #--------------------------------------------------------- file methods ----
    #' @description
    #' Set the file names for data and meta data.
    #' 
    #' @param data_files character(), containing the full file names.
    #' @param meta_file character(1), containing the full meta file name.
    #' 
    set_files = function(data_files = NULL, meta_file = NULL) {
      cli::cli_h3("Setting files:")
      cli::cli_ul()
      if(!is.null(data_files)) {
        cli::cli_li("data files")
        self$files$data_files <- data_files
        private$add_log(message = "Set data file(s)")
      }
      if(!is.null(meta_file)) {
        cli::cli_li("meta data file")
        self$files$meta_filename <- meta_file
        private$add_log(message = "Set meta data file")
      }
      cli::cli_end()
      cli::cli_alert_success("Done!")
    },
    
    
    #------------------------------------------------------ general methods ----
    #' @description
    #' This method imports all data and meta data.
    #' 
    import = function() {
      cli::cli_h3("Importing")
      cli::cli_ul()
      
      # import meta data
      cli::cli_li("meta data")
      private$add_log(message = "Importing meta data")
      private$read_meta_data()
      
      # import data
      cli::cli_li("experimental data")
      private$add_log(message = "Importing data")
      private$import_data()
      
      # extracting indices
      cli::cli_li("extracting indices")
      private$add_log(message = "Extracting indices")
      private$extract_indices()
      
      # extracting tables
      cli::cli_li("extracting tables")
      private$add_log(message = "Extracting tables")
      private$extract_tables()
      
      cli::cli_alert_success("Done!")
    },
    
    
    #' @description
    #' Method to set all parameters
    #' 
    #' @param ids list(), column names containing information about, sample id,
    #' feature id. Valid entries are: id_col_meta, id_col_data.
    #' @param columns list(), column names containing information about,
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
      cli::cli_h3("Setting parameters")
      cli::cli_ul()
      if(!is.null(ids)) {
        private$set_parameters_ids(params = ids)
        private$add_log(message = "Set parameters: id's")
        cli::cli_li("id's")
      }
      if(!is.null(columns)) {
        private$set_parameters_columns(params = columns)
        private$add_log(message = "Set parameters: columns")
        cli::cli_li("column names")
      }
      if(!is.null(regex)) {
        private$set_parameters_regex(params = regex)
        private$add_log(message = "Set parameters: regular expression")
        cli::cli_li("regular expressions")
      }
      if(!is.null(imputation)) {
        # private$set_parameters_imputation(params = imputation)
        private$add_log(message = "Set parameters: imputation")
        cli::cli_li("imputation")
      }
      if(!is.null(batch_correction)) {
        # private$set_parameters_batch_correction(params = batch_correction)
        private$add_log(message = "Set parameters: batch correction")
        cli::cli_li("batch correctoin")
      }
      if(!is.null(blank_filtering)) {
        # private$set_parameters_blank_filtering(params = blank_filtering)
        private$add_log(message = "Set parameters: blank filtering")
        cli::cli_li("blank filtering")
      }
      if(!is.null(normalization)) {
        # private$set_parameters_blank_normalization(params = normalization)
        private$add_log(message = "Set parameters: normalization")
        cli::cli_li("normalization")
      }
      cli::cli_end()
      
      cli::cli_alert_success("Done!")
    },
    
    
    #' @description
    #' Run the QCpool calculations. Calculate RSD and Trend of all features.
    #' 
    calc_qcpool = function() {
      cli::cli_h3("Calculating QCpool")
      cli::cli_ul()
      # calculate RSD
      cli::cli_li("RSD")
      private$add_log(message = "Calculating QCpool: rsd")
      private$calc_qcpool_rsd()
      
      # calculate trend
      cli::cli_li("trend")
      private$add_log(message = "Calculating QCpool: trend")
      private$calc_qcpool_trend()
      
      cli::cli_alert_success("Done!")
    },
    
    
    #' @description
    #' Show the histogram of RSD values.
    #'
    #' @param rsd_limit numeric(1), set a limit between 0 and 1.
    #'
    #' @return ggplot2 object, RSD histogram.
    #'
    #' @importFrom ggplot2 ggplot aes geom_histogram .data theme_minimal labs
    #'     geom_vline guide_legend theme
    #'
    show_rsd_plot = function(rsd_limit = 0.3) {
      if(!is.null(self$tables$plot_rsd_data)) {
        p <- self$tables$plot_rsd_data |> 
          ggplot2::ggplot(ggplot2::aes(x = .data$rsd,
                                       fill = .data$polarity)) +
          ggplot2::geom_histogram(binwidth = 0.01,
                                  alpha = 0.5) +
          ggplot2::geom_vline(xintercept = rsd_limit,
                              color = "red",
                              linetype = 2) +
          ggplot2::guides(fill = ggplot2::guide_legend(title = "Polarity",
                                                       override.aes = list(alpha = 1))) +
          ggplot2::labs(x = "RSD") +
          ggplot2::theme_minimal() +
          ggplot2::theme(legend.position = "bottom")
        
        return(p)
      }
    },
    
    
    #' @description
    #' Show the trend plot.
    #'
    #' @return ggplot2 object, RSD histogram.
    #'
    #' @importFrom ggplot2 ggplot aes geom_line .data theme_minimal labs
    #'     guide_legend theme geom_hline
    #'
    show_trend_plot = function() {
      if(!is.null(self$tables$plot_trend_data)) {
        p <- self$tables$plot_trend_data |> 
          ggplot2::ggplot(ggplot2::aes(x = .data$sampleName,
                                       y = .data$log2fc,
                                       group = .data$id,
                                       colour = .data$polarity)) +
          ggplot2::geom_hline(yintercept = c(-0.5, 0, 0.5),
                              colour = c("black", "grey", "black"),
                              linetype = c(2, 1, 2)) +
          ggplot2::geom_line(alpha = 0.5) +
          ggplot2::guides(colour = ggplot2::guide_legend(title = "Polarity",
                                                         override.aes = list(alpha = 1))) +
          ggplot2::labs(x = "Sample id",
                        y = "Log2(fold change)") +
          ggplot2::theme_minimal() +
          ggplot2::theme(legend.position = "bottom",
                         axis.text.x = ggplot2::element_text(angle = 45,
                                                             hjust = 1))
        
        return(p)
      }
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
    #----------------------------------------------------- printing methods ----
    files_fun = function() {
      cli::cli_ul()
      cli::cli_li("Data files:")
      li_files <- cli::cli_ul()
      for(a in 1:length(self$files$data_files))
        cli::cli_li(self$files$data_files[a])
      cli::cli_end(li_files)
      cli::cli_li(paste("Meta data file:", self$files$meta_filename))
      cli::cli_end()
    },
    
    
    samples_fun = function() {
      cli::cli_ul()
      cli::cli_li(paste("NUmber of blanks:", length(self$indices$index_blanks)))
      cli::cli_li(paste("NUmber of qcs:", length(self$indices$index_qcs)))
      cli::cli_li(paste("NUmber of pools:", length(self$indices$index_pools)))
      cli::cli_li(paste("NUmber of samples:", length(self$indices$index_samples)))
      cli::cli_end()
    },
    
    
    #-------------------------------------------------------------- logging ----
    add_log = function(message = NULL) {
      if(is.null(self$history$id)) {
        id <- 0
      } else {
        id <- self$history$id[nrow(self$history)] + 1
      }
      self$history <- rbind.data.frame(
        self$history,
        data.frame(id = id,
                   datetime = format(Sys.time(), "%Y-%m-%d %H:%M:%OS3"),
                   message = message)
      )
    },
    
    
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
    # @description
    # Set the id (sampleid) columns for the meta data and the experimental data. 
    #
    # @param params list(), containing all the id types.
    #
    # @details 
    # Entry names of `params` are: `id_col_meta` and `id_col_data.`
    #
    set_parameters_ids = function(params = NULL) {
      if(!is.null(params$id_col_meta)) {
        self$indices$id_col_meta <- params$id_col_meta
      }
      if(!is.null(params$id_col_data)) {
        self$indices$id_col_data <- params$id_col_data
      }
    },
    
    # @description
    # Set the name of the different columns. 
    #
    # @param params list(), containing all the different column names.
    #
    # @details 
    # Entry names of `params` are: `type_column`, `group_column`, `batch_column`, and 
    # `order_column.`
    #
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
    
    # @description
    # Set the regular expressions to recognize the blanks, qcs, pools and samples.
    #
    # @param params list(), containing all the different regular expressions.
    #
    # @details 
    # Entry names of `params` are: `blanks`, `qcs`, `pools`, and `samples`
    #
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
    
    
    # @description
    # Extract the indices for the blanks, qcs, pools and samples.
    #
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
    
    # @description
    # Extract the tables blanks, qcs, pools and samples.
    #
    extract_tables = function() {
      if(!is.null(self$indices$index_blanks)) {
        self$tables$blank_data <- 
          self$tables$all_data[self$tables$all_data[, self$indices$id_col_data] %in% self$indices$index_blanks, ]
        self$tables$blank_data_long <- 
          self$tables$all_data_long[self$tables$all_data_long[, self$indices$id_col_data] %in% self$indices$index_blanks, ]
      }
      
      if(!is.null(self$indices$index_qcs)) {
        self$tables$qc_data <- 
          self$tables$all_data[self$tables$all_data[, self$indices$id_col_data] %in% self$indices$index_qcs, ]
        self$tables$qc_data_long <- 
          self$tables$all_data_long[self$tables$all_data_long[, self$indices$id_col_data] %in% self$indices$index_qcs, ]
      }
      
      if(!is.null(self$indices$index_pools)) {
        self$tables$pool_data <- 
          self$tables$all_data[self$tables$all_data[, self$indices$id_col_data] %in% self$indices$index_pools, ]
        self$tables$pool_data_long <- 
          self$tables$all_data_long[self$tables$all_data_long[, self$indices$id_col_data] %in% self$indices$index_pools, ]
      }
      
      if(!is.null(self$indices$index_samples)) {
        self$tables$sample_data <- 
          self$tables$all_data[self$tables$all_data[, self$indices$id_col_data] %in% self$indices$index_samples, ]
        self$tables$sample_data_long <- 
          self$tables$all_data_long[self$tables$all_data_long[, self$indices$id_col_data] %in% self$indices$index_samples, ]
      }
    }
    
  )
)
