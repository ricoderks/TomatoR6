#' @title Import the meta data
#' 
#' @description
#' Import the meta data.
#' 
#' @param self self 
#'
#' @returns self
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
#' @returns self
#' 
import_read_rawdata <- function(self = NULL) {
  data_df <- data.frame()
  
  for(a in 1:length(self$file_data)) {
    tmp <- read_msdial(file = self$file_data[a])
    
    data_df <- rbind.data.frame(data_df, tmp)
  }
  
  data_df <- cleanup(data_df = data_df)
  self$table_rawdata <- data_df
  
  # # get the features and information
  # private$extract_feature_data(data_df = data_df)
  # 
  # convert raw data table
  data_long <- make_table_long(data_wide = data_df)
  data_wide <- make_table_wide(data_long = data_long)
  
  self$table_alldata_long <- data_long
  self$table_alldata <- data_wide
  
  return(self)
}


#' @title Read a MSDIAL report file
#' 
#' @description
#' Read a MSDIAL report file.
#' 
#' @param file character(1), full path to result file.
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
                               quote = "\"")
  
  return(data_df)
}

#' @title Cleanup import MSDIAL results
#' 
#' @description
#' Cleanup import MSDIAL results.
#' 
#' @param data_df data.frame, containing the raw imported MSIDAL results.
#' 
#' @returns cleaned up data.frame
#' 
cleanup = function(data_df = NULL) {
  # add unique ID
  data_df$polarity <- ifelse(grepl(pattern = "\\+$",
                                   x = data_df$Adduct.type),
                             "pos",
                             "neg")
  
  data_df$id <- paste0(data_df$polarity, "_", data_df$Alignment.ID)
  
  # remove unknowns and others
  data_df <- data_df[!(grepl(pattern = "(unknown|no MS2|low score)",
                             x = data_df$Metabolite.name,
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
#' @param data_wide data.frame in wide format.
#' 
#' @returns data.frame in long format.
#' 
#' @importFrom tidyr pivot_longer
#' 
make_table_long = function(data_wide = NULL) {
  data_long <- data_wide[, c("id", colnames(data_wide)[grepl(pattern = "(blank|qcpool|sample)_",
                                                             x = colnames(data_wide),
                                                             ignore.case = TRUE)])] |> 
    tidyr::pivot_longer(
      cols = colnames(data_wide)[grepl(pattern = "(blank|qcpool|sample)_",
                                       x = colnames(data_wide),
                                       ignore.case = TRUE)],
      names_to = "sampleName",
      values_to = "peakArea"
    )
  
  return(as.data.frame(data_long))
}


#' @title Make data.frame wide
#' 
#' @description
#' Make data.frame wide.
#' 
#' @param data_long data.frame in long format.
#' 
#' @returns data.frame in wide format.
#' 
#' @importFrom tidyr pivot_wider
#' 
make_table_wide = function(data_long = NULL) {
  data_wide <- data_long |> 
    tidyr::pivot_wider(
      id_cols = "sampleName",
      names_from = "id",
      values_from = "peakArea"
    )
  
  return(data_wide)
}