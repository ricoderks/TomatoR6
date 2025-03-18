#' @title Total area normalisation
#' 
#' @description
#' Total area normalisation.
#' 
#' @param self object of class DataImport.
#' 
#' @noRd
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
  
  tot_value <- as.data.frame(tapply(data_long$value, list(data_long$sampleId), sum, na.rm = TRUE))
  tot_value$sampleId <- rownames(tot_value)
  colnames(tot_value)[1] <- "totalValue"
  
  data_long <- merge(
    x = data_long,
    y = tot_value,
    by = "sampleId"
  )
  
  data_long$normValue <- data_long$value / data_long$totalValue
  data_long <- data_long[, c("featureId", "sampleId", "normValue")]
  colnames(data_long)[3] <- "value"
  
  self$table_analysis_long <- data_long
  
  return(invisible(self))
}


#' @title PQN normalisation
#' 
#' @description PQN normalisation.
#'
#' @param self object of class DataImport.
#'
#' @importFrom tidyr pivot_longer
#'
#' @noRd
#'
#' @returns self (invisible)
#' 
norm_pqn <- function(self = NULL) {
  self$norm_pqn_reference <- match.arg(arg = self$norm_pqn_reference,
                                       choices = c("median", "mean"))
  
  data_wide <- self$table_analysis
  data_norm <- data_wide
  rownames(data_wide) <- data_wide$sampleId
  
    
  QC <- which(rownames(data_wide) %in% self$index_pools)
  data_norm[, -1] <- pqn(X = data_wide[, -1],
                         n = self$norm_pqn_reference,
                         QC = QC)
  
  self$table_analysis <- data_norm
  
  self$table_analysis_long <- data_norm |> 
    tidyr::pivot_longer(cols = colnames(data_norm)[-1],
                        names_to = "featureId",
                        values_to = "peakArea")
  
  return(invisible(self))
}


#' @title Perform Probabilistic Quotient Normalization
#' 
#' @description Perform Probabilistic Quotient Normalization
#'
#' @param X matrix to normalize samples * variables (rows * columns)
#' @param n normalization reference: "mean" for using the overall average of variables as reference 
#' or "median" (default) for using the overall median of variables as reference
#' @param QC vector of number(s) to specify samples which average to use as reference 
#' (e.g. QC samples)
#'
#' @returns Normalized table samples * variables (rows * columns)
#' 
#' @details First a total area normalization should be done before PQN is applied.
#' 
#' @importFrom stats median
#' 
#' @noRd
#'
#' @author E. Nevedomskaya
#' @author Rico Derks
#' 
#' @references Dieterle, F., Ross, A., Schlotterbeck, G. & Senn, H. Probabilistic Quotient 
#' Normalization as Robust Method to Account for Dilution of Complex Biological Mixtures. 
#' Application in H1 NMR Metabonomics. Anal. Chem. 78, 4281-4290 (2006).
#' 
pqn <- function(X = NULL, n = "median", QC = NULL) {
  X.norm <- matrix(nrow = nrow(X), ncol = ncol(X))
  colnames(X.norm) <- colnames(X)
  rownames(X.norm) <- rownames(X)
  
  if (!is.null(QC)) {
    # if QC vector exists, use this as reference spectrum
    if (length(QC) == 1) {
      # only 1 reference sample given
      mX <- as.numeric(X[QC, ])
    } else {
      if (n == "mean") {
        mX <- as.numeric(colMeans(X[QC, ]))
      }
      if (n == "median") {
        mX <- as.numeric(apply(X[QC, ], 2, median))
      }
    }
  } else {
    # otherwise use the mean or median of all samples as reference sample
    if (n == "mean") {
      mX <- as.numeric(colMeans(X))
    }
    if (n == "median") {
      mX <- as.numeric(apply(X, 2, median))
    }
  }
  
  # do the actual normalisation
  for (a in 1:nrow(X)) {
    X.norm[a, ] <- as.numeric(X[a, ] / median(as.numeric(X[a, ] / mX)))
  }
  
  return(X.norm)
}