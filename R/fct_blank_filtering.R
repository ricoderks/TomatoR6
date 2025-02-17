#' @title Calculate the sample / blank ratio
#' 
#' @description
#' Calculate sample / blank ratio..
#' 
#' @param self object of class DataImport.
#' 
#' @returns self (invisible).
#' 
blank_calc_ratio <- function(self = NULL) {
  feature_data <- self$table_featuredata
  blank_data <- self$table_blank_long
  blank_index <- self$index_blanks
  sample_data <- self$table_sample_long
  sample_index <- self$index_samples
  
  # blank_data <- obj$table_blank_long
  # sample_data <- obj$table_sample_long
  
  blank_data_avg <- tapply(blank_data, list(blank_data[, "id"]), function(x) {
    avg <- mean(x[, "peakArea"], na.rm = TRUE)
    
    return(data.frame("id" = x[1, "id"],
                      "avgPeakArea" = avg))
  })
  blank_data_avg <- do.call("rbind", blank_data_avg)
  
  sample_data <- merge(
    x = sample_data,
    y = blank_data_avg,
    by = "id"
  )
  
  sample_data$ratio <- sample_data$peakArea / sample_data$avgPeakArea
  
  self$table_blank_filtering <- sample_data
  
  return(invisible(self))
}


blank_apply_filter <- function(self = NULL) {
  if(!is.null(self$table_blank_filtering)) {
    blank_data <- self$table_blank_filtering
    ratio <- self$blank_ratio
    threshold <- self$blank_threshold
    
    # blank_data <- obj$table_blank_filtering
    # ratio <- obj$blank_ratio
    # threshold <- obj$blank_threshold
    
    blank_data$keep <- blank_data$ratio >= ratio
    blank_data$keep[is.na(blank_data$keep)] <- FALSE
    
    features <- tapply(blank_data, list(blank_data[, "id"]), function(x) {
      prop <- mean(x[, "keep"])
      return(data.frame( "id" = x[1, "id"],
                        propertion = prop))
    })
    features <- do.call("rbind", features)
    
    keep <- features$id[features$propertion >= threshold]
    self$table_featuredata$keep_sample_blank <- self$table_featuredata$id %in% keep
    
    return(invisible(self))
  }
}



