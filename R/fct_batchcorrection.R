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
                         batch_col = self$batch_column),
    "loess" = bc_loess(data = self$table_analysis,
                       meta_data = self$table_metadata,
                       id_col_meta = self$id_col_meta,
                       id_qcpool = self$index_pools,
                       batch_col = self$batch_column,
                       order_col = self$order_column,
                       span = self$bc_loess_span,
                       method = self$bc_loess_method)
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
#' @param data data.frame in wide format.
#' @param meta_data data.frame with the meta data.
#' @param id_col_meta character(1), name of the sample id column in the meta data.
#' @param id_qcpool character() vector with the names of the pooled sample id's.
#' @param batch_col character(1), name of the batch column.
#' @param order_col character(1), name of the acquisition order column.
#' @param span numeric(1), the parameter alpha which controls the degree of smoothing.
#' @param method character(1), perform de batch correction per batch or over all batches at once.
#'
#' @return data.frame with LOESS batch corrected data.
#'
#' @author Rico Derks
#'
#' @noRd
#' 
bc_loess <- function(data = NULL,
                     meta_data = NULL,
                     id_col_meta = NULL,
                     id_qcpool = NULL,
                     batch_col = NULL,
                     order_col = NULL,
                     span = 0.75,
                     method = c("batch", "over_all")) {
  
  method <- match.arg(arg = method,
                      choices = c("batch", "over_all"))
  
  # data <- obj$table_analysis
  # meta_data <- obj$table_metadata
  # id_col_meta <- obj$id_col_meta
  # batch_col <- obj$batch_column
  # order_col <- obj$order_column
  # method <- obj$bc_loess_method
  # id_qcpool <- obj$index_pools
  
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
  
  cor_data <- cor_data[order(cor_data[, order_col]), ]
  
  if(all(!is.na(cor_data[id_qcpool, feature_names]))) {
    switch(
      method,
      "batch" = {
        for(b in 1:length(batches)) {
          cor_data[cor_data[, batch_col] == batches[b], feature_names] <-
            qc_rlsc(
              tab = cor_data[cor_data[, batch_col] == batches[b], feature_names],
              colv = ifelse(cor_data[cor_data[, batch_col] == batches[b], "sampleName"] %in% id_qcpool,
                            1,
                            2),
              or = cor_data[cor_data[, batch_col] == batches[b], order_col],
              span = span)
        }
      },
      "over_all" = {
        cor_data[, feature_names] <-
          qc_rlsc(
            tab = cor_data[, feature_names],
            colv = ifelse(cor_data[, "sampleName"] %in% id_qcpool,
                          1,
                          2),
            or = cor_data[, order_col],
            span = span)
      }
    )
    
    cor_data <- cor_data[, c("sampleName", feature_names)]
    cor_data_long <- utils_make_analysis_long(df = cor_data)
    
    return(list("wide" = cor_data,
                "long" = cor_data_long))
  } else {
    cli::cli_abort("There are NA's in the QCpools! Not possible to do correction!")
  }
}


#' @title Perform QC-RLSC for batch correction of chromatographic signal
#'
#' @description Perform QC-RLSC for batch correction of chromatographic signal.
#'
#' @param tab table N*K (row * column) with N samples and K variables.
#' @param colv vector N of numbers: 1 for QC samples and 2 for other samples.
#' @param or vector of measuring order (see details).
#' @param span the parameter alpha hich controls the degree of smoothing.
#' @param verbose print which variable has been corrected to monitor the process (default = FALSE).
#'
#' @return corrected table N*K
#'
#' @details Make sure that everything is sorted in measurement order!!!
#'
#' @importFrom stats loess approx
#'
#' @author E. Nevedomskaya
#' @author Rico Derks
#'
#' @references Dunn et al. Nature Protocols 6, 1060-1083 (2011)
#'
#' @noRd
qc_rlsc <- function(tab, colv, or, span = 0.75, verbose = FALSE) {
  # create table of the same size as initial
  tab_corr <- tab
  
  # For each variable (columns) in the initial table
  for (i in 1:ncol(tab)) {
    # fit loess curve to the QCs
    ll <- stats::loess(tab[which(colv == 1), i] ~ or[which(colv == 1)], span = span)
    
    # approximate the curve for all the samples
    aa <- stats::approx(x = or[which(colv == 1)],
                        y = ll$fitted,
                        xout = or)
    
    # correct the variable according to the curve for all the samples
    tab_corr[, i] <- tab[, i] / aa$y
    
    # print which variable has been corrected in order to monitor the progress
    if(verbose == TRUE) {
      print(i)
    }
    
  }
  
  return(tab_corr)
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
