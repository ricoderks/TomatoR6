#' @title Impute `NA` values
#' 
#' @description
#' Impute `NA` values.
#' 
#' @param self class object DataImport.
#' @param private private part of class object DataImport.
#' 
#' @returns self (invisible).
#' 
#' @noRd
#' 
impute_na <- function(self = NULL,
                      private = NULL) {
  data_table <- self$table_analysis
  data_table_long <- self$table_analysis_long
  features <- colnames(data_table[-1])
  
  impute_func <- private$impute_methods[[self$imp_method]]
  
  for(feature in features) {
    data_table[is.na(data_table[, feature]), feature] <-
      impute_func(data_table[, feature], na.rm = TRUE)
    data_table_long$peakArea[data_table_long$id == feature & 
                               is.na(data_table_long$peakArea[data_table_long$id == feature])] <- 
      impute_func(data_table_long$peakArea[data_table_long$id == feature], na.rm = TRUE)
  }  
  
  self$table_analysis <- data_table
  self$table_analysis_long <- data_table_long
  
  return(invisible(self))
}

