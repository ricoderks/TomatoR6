#' @title Import the meta data
#' 
#' @description
#' Import the meta data.
#' 
#' @param self self 
#'
#' @returns self
#' 
#' @noRd
#' 
#' @importFrom utils read.csv read.table
#' @importFrom openxlsx2 read_xlsx
#'
import_read_metadata <- function(self = NULL) {
  extension <- sub(pattern = ".*(csv|txt|xlsx)$",
                   replacement = "\\1",
                   x = self$file_meta)
  
  meta_df <- switch(
    extension,
    "csv" = utils::read.csv(file = self$file_meta,
                            header = TRUE),
    "txt" = utils::read.table(file = self$file_meta,
                              header = TRUE,
                              sep = "\t"),
    "xlsx" = openxlsx2::read_xlsx(file = self$file_meta)
  )
  
  self$table_metadata <- meta_df
  
  return(self)
}


#' @title Import raw data
#' 
#' @description
#' Import raw data.
#' 
#' @param self self 
#'
#' @noRd
#'
#' @returns self
#' 
import_read_msdial <- function(self = NULL) {
  data_df <- data.frame()
  
  for(a in 1:length(self$file_data)) {
    tmp <- read_msdial(file = self$file_data[a])
    
    data_df <- rbind.data.frame(data_df, tmp)
  }
  
  data_df <- cleanup(data_df = data_df)
  self$table_rawdata <- data_df
  
  return(self)
}


#' @title Read a MSDIAL report file
#' 
#' @description
#' Read a MSDIAL report file.
#' 
#' @param file character(1), full path to result file.
#' 
#' @noRd
#' 
#' @returns data.frame with MSDIAL results.
#' 
#' @importFrom utils read.table
#'
read_msdial = function(file = NULL) {
  data_df <- utils::read.table(file = file,
                               header = TRUE,
                               sep = "\t",
                               skip = 4,
                               comment.char = "",
                               quote = "\"",
                               check.names = FALSE)
  
  return(data_df)
}

#' @title Cleanup import MSDIAL results
#' 
#' @description
#' Cleanup import MSDIAL results.
#' 
#' @param data_df data.frame, containing the raw imported MSIDAL results.
#' 
#' @noRd
#' 
#' @returns cleaned up data.frame
#' 
cleanup = function(data_df = NULL) {
  # add unique ID
  data_df$polarity <- ifelse(grepl(pattern = "\\+$",
                                   x = data_df$`Adduct type`),
                             "pos",
                             "neg")
  
  data_df$id <- paste0(data_df$polarity, "_", data_df$`Alignment ID`)
  
  # remove unknowns and others
  data_df <- data_df[!(grepl(pattern = "(unknown|no MS2|low score)",
                             x = data_df$`Metabolite name`,
                             ignore.case = TRUE) |
                         grepl(pattern = "(unknown|others)",
                               x = data_df$Ontology,
                               ignore.case = TRUE)), ]
  
  return(data_df)
}


#' @title Make data.frame long
#' 
#' @description
#' Make data.frame long.
#' 
#' @param self data.frame in wide format.
#' 
#' @noRd
#' 
#' @returns self (invisible).
#' 
#' @importFrom tidyr pivot_longer
#' 
make_table_long = function(self = NULL) {
  data_wide <- self$table_rawdata
  sample_col_names <- c(self$index_blanks, self$index_qcs, self$index_pools, self$index_samples)
  
  data_long <- data_wide[, c("id", sample_col_names)] |> 
    tidyr::pivot_longer(
      cols = sample_col_names,
      names_to = "sampleName",
      values_to = "peakArea"
    )
  
  self$table_alldata_long <- as.data.frame(data_long)
  self$table_analysis_long <- as.data.frame(data_long)
  
  invisible(self)
}


#' @title Make data.frame wide
#' 
#' @description
#' Make data.frame wide.
#' 
#' @param self data.frame in long format.
#' 
#' @noRd
#' 
#' @returns self (invisible).
#' 
#' @importFrom tidyr pivot_wider
#' 
make_table_wide = function(self = NULL) {
  data_long <- self$table_alldata_long
  
  data_wide <- data_long |> 
    tidyr::pivot_wider(
      id_cols = "sampleName",
      names_from = "id",
      values_from = "peakArea"
    )
  data_wide <- as.data.frame(data_wide)
  rownames(data_wide) <- data_wide$sampleName
  
  self$table_alldata <- data_wide
  self$table_analysis <- data_wide
  
  invisible(self)
}


#' @title Extract lipid feature data
#' 
#' @description
#' Extract lipid feature data.
#' 
#' @param self class object
#' 
#' @noRd
#' 
#' @returns self (invisible)
#' 
extract_lipid_data = function(self = NULL) {
  data_df <- self$table_rawdata
  
  data_df <- data_df[, c("id", "Metabolite name", "Ontology", "Adduct type", "Average Rt(min)", "Average Mz")]
  split_name <- strsplit(x = data_df$`Metabolite name`,
                         split = "\\|")
  
  short <- vector(mode = "character",
                  length = length(split_name))
  long <- vector(mode = "character",
                 length = length(split_name))
  for(a in 1:length(split_name)) {
    if(length(split_name[[a]]) == 1) {
      short[a] <- split_name[[a]][1]
      long[a] <- split_name[[a]][1]
    } else {
      short[a] <- split_name[[a]][1]
      long[a] <- split_name[[a]][2]
    }
  }
  
  data_df$shortLipidName <- short
  data_df$longLipidname <- long
  # for now keep all features
  data_df$keep <- TRUE
  data_df$keep_rsd <- TRUE
  data_df$keep_sample_blank <- TRUE
  
  self$table_featuredata <- data_df
  
  return(invisible(self))
}


#' @title Extract metabolite data
#' 
#' @description
#' Extract metabolite data.
#' 
#' @param self class object
#' 
#' @noRd
#' 
#' @returns self (invisible)
#' 
extract_metabolite_data = function(self = NULL) {
  data_df <- self$table_rawdata
  
  data_df <- data_df[, c("id", "Metabolite name", "Ontology", "Adduct type", "Average Rt(min)", "Average Mz")]
  
  # for now keep all features
  data_df$keep <- TRUE
  data_df$keep_rsd <- TRUE
  data_df$keep_sample_blank <- TRUE
  
  self$table_featuredata <- data_df
  
  return(invisible(self))
}


#' @title Extract indices from the data
#' 
#' @description
#' Extract indices from the data.
#' 
#' @param self class object
#' 
#' @noRd
#' 
#' @returns self (invisible)
#' 
extract_indices <- function(self = self) {
  if(!is.null(self$table_metadata) & !is.null(self$id_col_meta)) {
    if(!is.null(self$regex_blanks)) {
      self$index_blanks <- 
        self$table_metadata[grep(pattern = self$regex_blanks,
                                 x = self$table_metadata[, self$type_column], #should this now be type_column
                                 ignore.case = TRUE), self$id_col_meta]
    }
    
    if(!is.null(self$regex_qcs)) {
      self$index_qcs <- 
        self$table_metadata[grep(pattern = self$regex_qcs,
                                 x = self$table_metadata[, self$type_column],
                                 ignore.case = TRUE), self$id_col_meta]
    }
    
    if(!is.null(self$regex_pools)) {
      self$index_pools <- 
        self$table_metadata[grep(pattern = self$regex_pools,
                                 x = self$table_metadata[, self$type_column],
                                 ignore.case = TRUE), self$id_col_meta]
    }
    
    if(!is.null(self$regex_samples)) {
      self$index_samples <- 
        self$table_metadata[grep(pattern = self$regex_samples,
                                 x = self$table_metadata[, self$type_column],
                                 ignore.case = TRUE), self$id_col_meta]
    }
  }
  
  return(invisible(self))
}

