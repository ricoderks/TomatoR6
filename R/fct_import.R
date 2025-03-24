#' @title Import the meta data
#' 
#' @description
#' Import the meta data.
#' 
#' @param self class object DataImport 
#'
#' @returns self (invisible).
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
  
  return(invisible(self))
}


#' @title Import raw data
#' 
#' @description
#' Import raw data.
#' 
#' @param self class object UntargetedLipidomics 
#'
#' @noRd
#'
#' @returns self (invisible).
#' 
import_read_msdial <- function(self = NULL) {
  data_df <- data.frame()
  
  for(a in 1:length(self$file_data)) {
    tmp <- read_msdial(file = self$file_data[a])
    
    data_df <- rbind.data.frame(data_df, tmp)
  }
  
  data_df <- cleanup(data_df = data_df)
  self$table_rawdata <- data_df
  
  return(invisible(self))
}


#' @title Import lipidyzer data
#' 
#' @description
#' Import lipidyzer data from (multiple) file(s).
#' 
#' @param self class object TargetedLipidomics
#' 
#' @details
#' Multiple files means that these are results from multiple batches from the 
#' same study. These need to be merged, but can have different amount of columns.
#' This is fixed here.
#' 
#' @noRd
#' 
#' @returns self (invisible).
#' 
import_lipidyzer <- function(self = self) {
  data_df <- data.frame()
  
  for(a in 1:length(self$file_data)) {
    tmp <- read_lipidyzer(file = self$file_data[a],
                          sheet = self$lipidyzer_sheet)
    
    if(any(dim(data_df)[1L])) {
      tmp[setdiff(names(data_df), names(tmp))] <- NA
      data_df[setdiff(names(tmp), names(data_df))] <- NA
    }
    
    data_df <- rbind.data.frame(data_df, tmp)
  }
  colnames(data_df)[1] <- "sampleId"
  order_cols <- sort(colnames(data_df)[-1])
  data_df <- data_df[, c("sampleId", order_cols)]
  self$table_rawdata <- data_df
  
  return(invisible(self))
}


#' @title Import MultiQuant data
#' 
#' @description
#' Import MulitQuant data.
#' 
#' @param self class object.
#' 
#' @returns self (invisible).
#' 
#' @importFrom stats na.omit
#' 
#' @noRd
#' 
import_multiquant <- function(self = NULL) {
  keep_columns <- c(
    "Sample Index", "Sample Name", "Sample Type",
    "Acquisition Date & Time", "Component Name", "Component Index", "Polarity",
    "IS", "IS Name", "IS Retention Time", "IS Area", "Actual Concentration",
    "Area", "Area Ratio", "Height", "Retention Time", "Relative RT", 
    "Calculated Concentration"
  )
  
  data_df <- read.table(
    file = self$file_data,
    header = TRUE,
    sep = "\t",
    colClasses = c(
      "Index" = "integer",
      "Sample Index" = "character",
      "Sample Name"= "character",
      "Sample Type" = "character",
      "Acquisition Date & Time" = "character",
      "Component Name" = "character",
      "Component Index" = "integer",
      "Polarity" = "character",
      "IS" = "logical",
      "IS Name" = "character",
      "IS Retention Time" = "numeric",
      "IS Area" = "numeric",
      "Actual Concentration" = "numeric",
      "Area" = "numeric",
      "Area Ratio" = "numeric",
      "Height" = "numeric",
      "Retention Time" = "numeric",
      "Relative RT" = "numeric",
      "Calculated Concentration" = "character"
    ),
    na.strings = c("", "NA", "N/A"),
    check.names = FALSE
  )
  
  data_df <- data_df[, keep_columns]
  colnames(data_df) <- c("sampleId", "sampleName", "sampleType", "acqDateTime",
                         "featureName", "featureId", "polarity", "istd", 
                         "istdFeatureName", "istdRt", "istdPeakArea", "actualConc", 
                         "peakArea", "areaRatio", "peakHeight", "rt", "relativeRt",
                         "calculatedConc")
  data_df$acqDateTime <- as.POSIXct(data_df$acqDateTime,
                                    format = "%m/%d/%Y %H:%M:%OS",
                                    tz = "UTC")
  # some fixes
  data_df$calculatedConc <- as.numeric(gsub(x = data_df$calculatedConc,
                                            pattern = "^(\\d*\\.\\d*)?.*$",
                                            replacement = "\\1"))
  data_df$polarity <- ifelse(data_df$polarity == "Negative",
                             "neg",
                             "pos")
  data_df$rt <- data_df$rt * 60
  data_df$mz <- NA_real_
  
  # internal standard stuff
  data_df <- data_df[!data_df$istd, ]
  data_df$istdFeatureName[data_df$istdFeatureName == "(No IS)"] <- NA
  
  istd_df <- data.frame(
    istdFeatureName = stats::na.omit(unique(data_df$istdFeatureName))
  )
  if(nrow(istd_df) > 0) {
    istd_df$istdFeatureId <- 1:nrow(istd_df)
    
    data_df <- merge(
      x = data_df,
      y = istd_df,
      by = "istdFeatureName",
      all.x = TRUE
    )
  } else {
    data_df$istdFeatureId <- NA
  }
  
  # injection order
  sampleIds <- unique(data_df$sampleId[order(data_df$acqDateTime)])
  for(a in 1:length(sampleIds)) {
    data_df$injOrder[data_df$sampleId == sampleIds[a]] <- as.integer(a)
  }
  
  # final cleanup
  data_df$istd <- NULL
  
  self$table_rawdata <- data_df
  
  return(invisible(self))
}


#' @title Import MultiQuant curation data
#' 
#' @description
#' Import MulitQuant curation data.
#' 
#' @param self class object.
#' 
#' @returns self (invisible).
#' 
#' @importFrom openxlsx2 read_xlsx
#' 
#' @noRd
#' 
import_curation_mq <- function(self = NULL) {
  data_df <- openxlsx2::read_xlsx(file = self$file_curation,
                                  sheet = 1)
  colnames(data_df)[5:6] <- c("cl", "clComments")
  
  self$table_curation <- data_df
  
  return(invisible(self))
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
read_msdial <- function(file = NULL) {
  data_df <- utils::read.table(file = file,
                               header = TRUE,
                               sep = "\t",
                               skip = 4,
                               comment.char = "",
                               quote = "\"",
                               check.names = FALSE)
  
  return(data_df)
}


#' @title Read lipidyzer file
#' 
#' @description
#' Read a lipidyzer result file.
#' 
#' @param file character(1) full path to result file.
#' @param sheet numeric(1) which sheet to read.
#' 
#' @noRd
#' 
#' @returns data.frame with lipidyzer results.
#' 
#' @importFrom openxlsx2 read_xlsx
#' 
read_lipidyzer <- function(file = NULL,
                           sheet = 1) {
  
  data_df <- openxlsx2::read_xlsx(file = file,
                                  sheet = sheet,
                                  row_names = FALSE,
                                  col_names = TRUE,
                                  na.strings = "",
                                  check_names = FALSE)
  data_df[, -1] <- apply(data_df[, -1], 2, as.numeric)
  
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
cleanup <- function(data_df = NULL) {
  # add unique ID
  data_df$polarity <- ifelse(grepl(pattern = "\\+$",
                                   x = data_df$`Adduct type`),
                             "pos",
                             "neg")
  
  data_df$featureId <- paste0(data_df$polarity, "_", data_df$`Alignment ID`)
  
  # remove unknowns and others
  data_df <- data_df[!(grepl(pattern = "(unknown|no MS2|low score)",
                             x = data_df$`Metabolite name`,
                             ignore.case = TRUE) |
                         grepl(pattern = "(unknown|others)",
                               x = data_df$Ontology,
                               ignore.case = TRUE)), ]
  
  return(data_df)
}


#' @title Make data.frame with MS-DIAL results long
#' 
#' @description
#' Make data.frame with MS-DIAL results long.
#' 
#' @param self data.frame in wide format.
#' 
#' @noRd
#' 
#' @returns self (invisible).
#' 
#' @importFrom tidyr pivot_longer all_of
#' 
make_table_long_msdial <- function(self = NULL) {
  data_wide <- self$table_rawdata
  sample_col_names <- c(self$index_blanks, self$index_qcs, self$index_pools, self$index_samples)
  
  data_long <- data_wide[, c("featureId", sample_col_names)] |> 
    tidyr::pivot_longer(
      cols = tidyr::all_of(sample_col_names),
      names_to = "sampleId",
      values_to = "value"
    )
  
  self$table_alldata_long <- as.data.frame(data_long)
  self$table_analysis_long <- as.data.frame(data_long)
  
  return(invisible(self))
}


#' @title Make data.frame with MS-DIAL results wide
#' 
#' @description
#' Make data.frame with MS-DIAL results wide.
#' 
#' @param self data.frame in long format.
#' 
#' @noRd
#' 
#' @returns self (invisible).
#' 
#' @importFrom tidyr pivot_wider
#' 
make_table_wide <- function(self = NULL) {
  data_long <- self$table_alldata_long
  
  data_wide <- data_long |> 
    tidyr::pivot_wider(
      id_cols = "sampleId",
      names_from = "featureId",
      values_from = "value"
    )
  data_wide <- as.data.frame(data_wide)
  rownames(data_wide) <- data_wide$sampleId
  
  self$table_alldata <- data_wide
  self$table_analysis <- data_wide
  
  return(invisible(self))
}


#' @title Make data.frame with lipidyzer results long
#' 
#' @description
#' Make data.frame with lipidyzer results long.
#' 
#' @param self data.frame in wide format.
#' 
#' @noRd
#' 
#' @returns self (invisible).
#' 
#' @importFrom tidyr pivot_longer all_of
#' 
make_table_long_lipidyzer <- function(self = NULL) {
  data_wide <- self$table_rawdata
  features <- colnames(data_wide)[-1]
  
  data_long <- data_wide |> 
    tidyr::pivot_longer(
      cols = tidyr::all_of(features),
      names_to = "featureId",
      values_to = "value"
    )
  
  data_long <- as.data.frame(data_long)
  featureId <- unique(data_long$featureId)
  ids <- 1:length(featureId)
  names(ids) <- featureId
  data_long$featureId <- ids[data_long$featureId]
  
  self$table_alldata_long <- data_long
  self$table_analysis_long <- data_long
  
  return(invisible(self))
}


#' @title Make data.frame with lipidyzer results long
#' 
#' @description
#' Make data.frame with lipidyzer results long.
#' 
#' @param self data.frame in wide format.
#' 
#' @noRd
#' 
#' @returns self (invisible).
#' 
#' @importFrom tidyr pivot_longer all_of
#' 
extract_table_long_mq <- function(self = NULL) {
  data_long <- self$table_rawdata

  data_col <- switch(
    self$data_to_extract,
    "Area" = "peakArea",
    "Area_Ratio" = "areaRatio",
    "peakArea"
  )
  
  data_long <- data_long[, c("featureId", "sampleId", data_col)]
  colnames(data_long)[3] <- "value"

  self$table_alldata_long <- data_long
  self$table_analysis_long <- data_long
  
  return(invisible(self))
}


#' @title Extract lipid feature data from MS-DIAL results
#' 
#' @description
#' Extract lipid feature data from MS-DIAL results.
#' 
#' @param self class object
#' 
#' @noRd
#' 
#' @returns self (invisible)
#' 
extract_lipid_data_msdial <- function(self = NULL) {
  data_df <- self$table_rawdata
  
  data_df <- data_df[, c("featureId", "Metabolite name", "Ontology", "Adduct type", "Average Rt(min)", "Average Mz")]
  colnames(data_df) <- c("featureId", "featureName", "class", "adductType", "averageRt", "averageMz")
  data_df$polarity <- ifelse(grepl(x = data_df$adductType,
                                   pattern = ".*\\+$"),
                             "pos",
                             "neg")
  
  split_name <- strsplit(x = data_df$featureName,
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
  
  data_df$shortFeatureName <- short
  data_df$longFeatureName <- long
  # for now keep all features
  data_df$keep <- TRUE
  data_df$keep_rsd <- TRUE
  data_df$keep_sample_blank <- TRUE
  
  self$table_featuredata <- data_df
  
  return(invisible(self))
}


#' @title Extract lipid feature data from lipidyzer results
#' 
#' @description
#' Extract lipid feature data from lipidyzer results.
#' 
#' @param self class object.
#' @param private private part of object.
#' 
#' @details
#' The feature information is in the column names of the data.frame.
#' 
#' @noRd
#' 
#' @returns self (invisible)
#' 
extract_lipid_data_lipidyzer <- function(self = NULL,
                                        private = NULL) {
  features <- colnames(self$table_rawdata)[-1]
  
  data_df <- data.frame("featureId" = 1:length(features),
                        "featureName" = features)
  
  if(self$split_PE) {
    class_pattern <- "^([a-zA-Z]*)( d18:0| d18:1)? ?(P-|O-)?.*"
    class_replacement <- "\\3\\1\\2"
    
  } else {
    class_pattern <- "^([a-zA-Z]*)( d18:0| d18:1)?.*"
    class_replacement <- "\\1\\2"
  }
  
  data_df$class <- gsub(x = data_df$featureName,
                        pattern = class_pattern,
                        replacement = class_replacement)
  
  data_df$polarity <- ifelse(data_df$class %in% private$lipid_class_neg,
                             "neg",
                             "pos")
  
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
extract_metabolite_data <- function(self = NULL) {
  data_df <- self$table_rawdata
  
  data_df <- data_df[, c("id", "Metabolite name", "Ontology", "Adduct type", "Average Rt(min)", "Average Mz")]
  colnames(data_df) <- c("featureId", "featureName", "class", "adductType", "averageRt", "averageMz")
  
  # for now keep all features
  data_df$keep <- TRUE
  data_df$keep_rsd <- TRUE
  data_df$keep_sample_blank <- TRUE
  
  self$table_featuredata <- data_df
  
  return(invisible(self))
}


#' @title Extract features from MultiQuant
#' 
#' @description
#' Extract featrues from MultiQuant
#' 
#' @param self class object.
#' 
#' @returns self (invissible).
#' 
#' @noRd
#' 
extract_features_mq <- function(self = NULL) {
  data_df <- self$table_rawdata
  
  data_df <- unique(data_df[, c("featureId", "featureName", "polarity", "istdFeatureName")])
  data_df$class <- NA
  
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
                                 x = self$table_metadata[, self$type_column],
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


#' @title Check meta data column
#' 
#' @description
#' Check if a column name is present in the meta data.
#' 
#' @param self class object.
#' 
#' @details
#' Checking the column names for id, sample type, batch, injection order and 
#' group.
#' 
#' @importFrom cli cli_abort
#' 
#' @returns logical(1) indicating if a column names are present.
#' 
#' @noRd
#' 
check_meta_column <- function(self = NULL) {
  column_names <- c(self$id_col_meta,
                    self$type_column,
                    self$batch_column,
                    self$order_column,
                    self$group_column)
  
  res <- column_names %in% colnames(self$table_metadata)
  
  if(!all(res)) {
      cli::cli_abort("Incorrect meta data column set!",
                     call = NULL)
  }
  
  return(all(res))
}


#' @title Fix the column names of the MultiQuant data.frame
#' 
#' @description
#' Fix the column names of the MultiQuant data.frame.
#' 
#' @param col_names character(), with the column names.
#' 
#' @returns character(), with corrected column names.
#' 
#' @noRd
#' 
mq_fix_column_names <- function(col_names = NULL) {
  col_names <- gsub(x = col_names,
                    pattern = " ",
                    replacement = "_")
  col_names <- gsub(x = col_names,
                    pattern = "&",
                    replacement = "and")
  col_names <- gsub(x = col_names,
                    pattern = "/",
                    replacement = "to")
  col_names <- gsub(x = col_names,
                    pattern = "%",
                    replacement = "percent")
  col_names <- gsub(x = col_names,
                    pattern = "#",
                    replacement = "number")
  
  return(col_names)
}


#' @title Extract meta data from MultiQuant data
#' 
#' @description
#' Extract meta data from MultiQuant data.
#' 
#' @details
#' The raw data is used to extract to injection order based on the acquisition 
#' date and time.
#' 
#' @param self  class object.
#' 
#' @returns self (invisible).
#' 
#' @noRd
#' 
extract_meta_from_raw <- function(self = NULL) {
  
  self$table_metadata <- merge(
    x = self$table_metadata,
    y = unique(self$table_rawdata[, c("sampleId", "sampleName", "injOrder")]),
    by = "sampleName",
    all.x = TRUE
  )
  
  return(invisible(self))  
}


#' @title Filter the data based on curation data
#' 
#' @description
#' Filter the data based on the data in the curation file.
#' 
#' @param self class object.
#' 
#' @returns self (invisible).
#' 
#' @noRd
#' 
filter_curation_mq <- function(self = NULL) {
  
  
  return(invisible(self))
}