#' @title Calculate the sample / blank ratio
#' 
#' @description
#' Calculate sample / blank ratio.
#' 
#' @param self object of class DataImport.
#' 
#' @noRd
#' 
#' @returns self (invisible).
#' 
#' @importFrom cli cli_abort
#' 
blank_calc_ratio <- function(self = NULL) {
  blank_index <- self$index_blanks
  if(length(blank_index) == 0) {
    cli::cli_abort(message = "No blanks specified!",
                   call = NULL)
  }
  feature_data <- self$table_featuredata
  sample_index <- self$index_samples
  blank_data <- self$table_analysis_long[self$table_analysis_long$sampleId %in% blank_index, ]
  sample_data <- self$table_analysis_long[self$table_analysis_long$sampleId %in% sample_index, ]
  
  blank_data_avg <- tapply(blank_data, list(blank_data[, "featureId"]), function(x) {
    avg <- mean(x[, "value"], na.rm = TRUE)
    
    return(data.frame("featureId" = x[1, "featureId"],
                      "avgValue" = avg))
  })
  blank_data_avg <- do.call("rbind", blank_data_avg)
  
  sample_data <- merge(
    x = sample_data,
    y = blank_data_avg,
    by = "featureId"
  )
  
  sample_data$ratio <- sample_data$value / sample_data$avgValue
  
  self$table_blank_filtering <- sample_data
  
  return(invisible(self))
}


#' @title Apply the blank filtering
#' 
#' @description
#' Apply the blank filtering.
#' 
#' @param self object of class DataImport.
#' 
#' @noRd
#' 
#' @returns self (invisible).
#' 
blank_apply_filter <- function(self = NULL) {
  if(!is.null(self$table_blank_filtering)) {
    blank_data <- self$table_blank_filtering
    ratio <- self$blank_ratio
    threshold <- self$blank_threshold
    threshold_group <- self$blank_group_threshold
    id_col_meta <- self$id_col_meta
    group_column <- self$group_column
    meta_data <- self$table_metadata[, c(id_col_meta, group_column)]
    
    blank_data <- merge(
      x = blank_data,
      y = meta_data,
      by.x = "sampleId",
      by.y = id_col_meta
    )
    
    # over all samples
    blank_data$keep <- blank_data$ratio >= ratio
    blank_data$keep[is.na(blank_data$keep)] <- FALSE
    
    features <- tapply(blank_data, list(blank_data[, "featureId"]), function(x) {
      prop <- mean(x[, "keep"])
      return(data.frame( "featureId" = x[1, "featureId"],
                        propertion = prop))
    })
    features <- do.call("rbind", features)
    keep <- features$id[features$propertion >= threshold]
    
    # groups
    features_groups <- as.data.frame(tapply(blank_data, list(blank_data[, "featureId"], blank_data[, group_column]), function(x) {
      prop <- mean(x[, "keep"])
    }))
    features_groups$keep <- apply(features_groups, 1, function(x) {
      any(x >= threshold_group)
    })
    keep_group <- rownames(features_groups)[features_groups$keep]
    
    keep_all <- union(keep, keep_group)
    self$table_featuredata$keep_sample_blank <- self$table_featuredata$featureId %in% keep_all
    
    return(invisible(self))
  }
}



