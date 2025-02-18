#' @title Add a log entry
#' 
#' @description
#' Add log entry into the history.
#' 
#' @param self object of class DataImport.
#' @param message character(1), log message.
#'
#' @noRd
#'
#' @returns self
#' 
utils_add_log <- function(self = NULL,
                          message = NULL) {
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
  invisible(self)
}


#' @title Show the files from DataImport class
#' 
#' @description
#' Show the files from DataImport class.
#' 
#' @param self object of class DataImport. 
#'
#' @noRd
#'
#' @import cli
#'
utils_file_show <-  function(self = NULL) {
  cli::cli_ul()
  cli::cli_li("Data files:")
  li_files <- cli::cli_ul()
  for(a in 1:length(self$.file_data))
    cli::cli_li(self$.file_data[a])
  cli::cli_end(li_files)
  cli::cli_li(paste("Meta data file:", self$.file_meta))
  cli::cli_end()
}


#' @title Show the samples from DataImport class
#' 
#' @description
#' Show the samples from DataImport class.
#' 
#' @param self object of class DataImport.
#'
#' @noRd
#'
#' @import cli
#'
utils_sample_show <-  function(self = NULL) {
  cli::cli_ul()
  cli::cli_li(paste("Number of blanks:", length(self$index_blanks)))
  cli::cli_li(paste("Number of qcs:", length(self$index_qcs)))
  cli::cli_li(paste("Number of pools:", length(self$index_pools)))
  cli::cli_li(paste("Number of samples:", length(self$index_samples)))
  cli::cli_end()
}


#' @title Determine which features to keep
#' 
#' @description
#' Determine which features to keep depending on the filters used.
#' 
#' @param self object of class DataImport.
#' 
#' @noRd
#' 
#' @returns self (invsible).
#' 
utils_analysis_features <- function(self = NULL) {
  for(a in 1:nrow(self$table_featuredata)) {
    self$table_featuredata$keep[a] <- all(
      self$table_featuredata$keep_rsd[a], 
      self$table_featuredata$keep_sample_blank[a]
    )
  }
  
  return(invisible(self))
}


#' @title Extract the sample data for further analysis
#' 
#' @description
#' Exctract the sample data table for further analysis. All set filtering 
#' steps have been applied on this table. 
#' 
#' @param self object of class DataImport.
#'
#' @noRd
#'
#' @returns self (invisible).
#'
utils_analysis_table <- function(self = NULL) {
  features <- self$table_featuredata$id[self$table_featuredata$keep == TRUE]
  observations <- c(self$index_blanks, self$index_qcs, self$index_pools, self$index_samples)
  
  # wide format
  self$table_analysis <- 
    self$table_analysis[self$table_analysis$sampleName %in% observations, c("sampleName", features)]
    
  # long format
  self$table_analysis_long <- 
    self$table_analysis_long[self$table_analysis_long$id %in% features &
                               self$table_analysis_long$sampleName %in% observations, ]
  
  return(invisible(self))
}


#' @title Reset the analysis table and feature table
#' 
#' @description
#' Reset the analysis table and feature table. 
#' 
#' @param self object of class DataImport.
#'
#' @noRd
#'
#' @returns self (invisible).
#'
utils_reset_tables <- function(self = NULL) {
  self$table_featuredata$keep <- TRUE
  self$table_featuredata$keep_rsd <- TRUE
  self$table_featuredata$keep_sample_blank <- TRUE
  
  self$table_analysis <- self$table_alldata
  self$table_analysis_long <- self$table_alldata_long
  
  return(invisible(self))
}

