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
  for(a in 1:length(self$file_data))
    cli::cli_li(self$file_data[a])
  cli::cli_end(li_files)
  cli::cli_li(paste("Meta data file:", self$file_meta))
  cli::cli_end()
}


#' @title Show the files from MultiQuant class
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
utils_file_show_mq <-  function(self = NULL) {
  cli::cli_ul()
  cli::cli_li("Data files:")
  li_files <- cli::cli_ul()
  for(a in 1:length(self$file_data))
    cli::cli_li(self$.file_data[a])
  cli::cli_end(li_files)
  cli::cli_li(paste("Meta data file:", self$file_meta))
  cli::cli_li(paste("Curation file:", self$file_curation))
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
  features <- self$table_featuredata$featureId[self$table_featuredata$keep == TRUE]
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


#' @title Make analysis table long
#' 
#' @description
#' make analysis table long.
#' 
#' @param df data.frame
#' 
#' @returns data.frame in long format
#' 
#' @importFrom tidyr pivot_longer all_of
#' 
#' @noRd
#' 
utils_make_analysis_long <- function(df = NULL) {
  df_long <- df |> 
    tidyr::pivot_longer(
      cols = tidyr::all_of(colnames(df)[-1]),
      names_to = "id",
      values_to = "peakArea"
    )
  
  df_long <- as.data.frame(df_long)
  
  return(df_long)
}


#' @title Calculate the additional lipidyzer tables
#' 
#' @description
#' Calculate the additional lipidyzer tables based on the Species concentration 
#' sheet.
#' 
#' @param self class object.
#' 
#' @details
#' To get correct results the Species Concentration sheet needs to be loaded. 
#' The following sheets will be calculated: Species composition, Class 
#' concentration, Class composition.
#' This is specifically useful after the Species concentration tables has been 
#' filtered, so use after `preprocessing()`.
#' 
#' @returns self (invisible), but with the extra tables added in wide and long format.
#' 
#' @importFrom cli cli_li
#' 
#' @noRd
#' 
utils_extract_additional_tables <- function(self = NULL) {
  species_conc_wide <- self$table_analysis
  feature_table <- self$table_featuredata
  
  # Species composition
  cli::cli_li("Extracting 'Species Composition'.")
  species_comp_wide <- species_conc_wide
  for(class in unique(feature_table$class)) {
    idx <- as.character(feature_table$featureId[feature_table$class == class])
    species_comp_wide[, idx] <- 
      species_comp_wide[, idx] / 
      rowSums(species_comp_wide[, idx, drop = FALSE], na.rm = TRUE) * 100
  }
  species_comp_long <- utils_make_analysis_long(df = species_comp_wide)
  
  # Class concentration
  cli::cli_li("Extracting 'Class Concentration'.")
  class_conc_wide <- data.frame(
    matrix(NA,
           nrow = nrow(species_conc_wide),
           ncol = length(unique(feature_table$class)) + 1)
  )
  colnames(class_conc_wide) <- c("sampleName", unique(feature_table$class))
  class_conc_wide$sampleName <- species_conc_wide$sampleName
  for(class in unique(feature_table$class)) {
    idx <- as.character(feature_table$featureId[feature_table$class == class])
    class_conc_wide[, class]  <- rowSums(species_conc_wide[, idx], na.rm = TRUE)
  }
  class_conc_wide$TG <- class_conc_wide$TG / 3
  class_conc_long <- utils_make_analysis_long(df = class_conc_wide)
  
  # Class composition
  cli::cli_li("Extracting 'Class Composition'.")
  class_comp_wide <- class_conc_wide
  class_comp_wide[, -1] <- class_comp_wide[, -1] / rowSums(class_comp_wide[, -1], na.rm = TRUE) * 100
  class_comp_long <- utils_make_analysis_long(df = class_comp_wide)
  
  self$table_species_comp <- species_comp_wide
  self$table_class_conc <- class_conc_wide
  self$table_class_comp <- class_comp_wide
  self$table_species_comp_long <- species_comp_long
  self$table_class_conc_long <- class_conc_long
  self$table_class_comp_long <- class_comp_long
  
  return(invisible(self))
}
