#' @title Total area normalisation
#' 
#' @description
#' Total area normalisation.
#' 
#' @param self object of class DataImport.
#' 
#' @returns self (invisible)
#' 
norm_total_area <- function(self = NULL) {
  # wide
  data_wide <- self$table_analysis
  
  tot_area <- rowSums(data_wide[, -1], na.rm = TRUE)
  data_wide[, -1] <- data_wide[, -1] / tot_area
  
  self$table_analysis <- data_wide
  
  # long
  data_long <- self$table_analysis_long
  
  tot_area <- as.data.frame(tapply(data_long$peakArea, list(data_long$sampleName), sum, na.rm = TRUE))
  tot_area$sampleName <- rownames(tot_area)
  colnames(tot_area)[1] <- "totalPeakArea"
  
  data_long <- merge(
    x = data_long,
    y = tot_area,
    by = "sampleName"
  )
  
  data_long$normPeakArea <- data_long$peakArea / data_long$totalPeakArea
  data_long <- data_long[, c("id", "sampleName", "normPeakArea")]
  colnames(data_long)[3] <- "peakArea"
  
  self$table_analysis_long <- data_long
  
  return(invisible(self))
}
