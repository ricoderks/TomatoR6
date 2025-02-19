#' @title Apply a batch correction method
#' 
#' @description
#' Apply a batch correction method
#' 
#' @param self class object DataImport.
#' @param private private part of class object DataImport.
#' 
#' @returns self (invisible).
#' 
#' @noRd
#' 
batchcorrection <- function(self = NULL) {
  bc_method <- self$bc_method
  
  res <- switch(
    EXPR = bc_method,
    "median" = bc_median(data = self$table_analysis,
                         meta_data = self$table_metadata,
                         id_col_meta = self$id_col_meta,
                         id_qcpool = self$index_pools,
                         batch_col = self$batch_column)
  )
  
  self$table_analysis <- res$wide
  self$table_analysis_long <- res$long
  
  return(invisible(self))
}


#' @title Median batch correction
#'
#' @description Median batch correction
#'
#' @param data data.frame in wide format.
#' @param meta_data data.frame with the meta data.
#' @param id_col_meta character(1), name of the sample id column in the meta data.
#' @param id_qcpool character() vector with the names of the pooled sample id's.
#' @param batch_col character(1), name of the batch column.
#'
#' @return data.frame with median batch corrected data.
#'
#' @importFrom stats median
#'
#' @author Rico Derks
#'
#' @noRd
#' 
bc_median <- function(data = NULL,
                      meta_data = NULL,
                      id_col_meta = NULL,
                      id_qcpool = NULL,
                      batch_col = NULL) {
  samples_selected <- rownames(data)
  feature_names <- colnames(data)[-1]
  
  data <- merge(
    x = data,
    y = meta_data,
    by.x = "sampleName",
    by.y = id_col_meta,
    all.x = TRUE
  )
  rownames(data) <- samples_selected
  
  # sort the columns
  other_columns <- colnames(data)[!(colnames(data) %in% feature_names)]
  data <- data[samples_selected, c(other_columns, feature_names)]
  
  cor_data <- data
  cor_data[, batch_col] <- factor(cor_data[, batch_col])
  batches <- as.character(unique(cor_data[, batch_col]))
  
  overall_median <- apply(as.matrix(cor_data[id_qcpool, feature_names]), 2, stats::median, na.rm = TRUE)
  
  for(b in 1:length(batches)) {
    idx <- cor_data[, batch_col] == batches[b]
    
    if(!all(idx == FALSE)) {
      tmp <- as.matrix(cor_data[idx & cor_data[, "sampleName"] %in% id_qcpool, feature_names])
      
      cor_factor <- overall_median / apply(tmp, 2, stats::median, na.rm = TRUE)
      
      cor_data[idx, feature_names] <- t(t(cor_data[idx, feature_names]) * cor_factor)
    }
  }
  
  cor_data <- cor_data[, c("sampleName", feature_names)]
  cor_data_long <- utils_make_analysis_long(df = cor_data)
  
  return(list("wide" = cor_data,
              "long" = cor_data_long))
}


#' @title Apply loess batch correctoin
#' 
#' @description
#' Apply loess batch correction
#'
#' @noRd 
#' 
bc_loess <- function() {
  
}


#' @title Apply combat batch correctoin
#' 
#' @description
#' Apply combat batch correction
#' 
#' @noRd 
#' 
bc_combat <- function() {
  
}
