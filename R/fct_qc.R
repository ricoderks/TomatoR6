#' @title Calculate the RSD values
#'
#' @description
#' Calculate the RSD values of all features.
#'
#' @param self object of class DataImport.
#' @param private private part of object.
#'
#' @importFrom stats sd
#'
#' @returns self (invisible).
#'
qc_calc_rsd <- function(self = NULL,
                       private = NULL) {
  if(!is.null(self$table_pool_long)) {
    feature_data <- self$table_featuredata
    pools_data <- self$table_pool_long
    qcpool_index <- self$index_pools
    
    pools_data <- pools_data[pools_data[, "sampleName"] %in% qcpool_index, ]
    
    rsd_data <- tapply(pools_data, list(pools_data[, "id"]), function(x) {
      rsd <- stats::sd(x[, "peakArea"], na.rm = TRUE) / mean(x[, "peakArea"], na.rm = TRUE)
      return(data.frame("id" = x[1, "id"],
                        "rsd" = rsd))
    })
    rsd_data <- do.call("rbind", rsd_data)
    
    rsd_data$polarity <- gsub(pattern = "(pos|neg).*",
                              replacement = "\\1",
                              x = rsd_data$id)
    rsd_data <- merge(
      x = rsd_data,
      y = feature_data[, c("id", "Ontology")],
      by = "id"
    )
    
    # if(is.null(self$.qc_rsd_limit)) {
    #   cli::cli_alert_danger("No RSD filtering applied, because there is no RSD limit set!")
    #   self$qc_rsd_limit <- 0
    #   private$add_log("No RSD limit set! Set to 0!")
    # }
    
    # no_keep <- rsd_data$id[rsd_data$rsd > self$.qc_rsd_limit]
    # self$tables$feature_data$keep_rsd <- self$tables$feature_data$id %in% no_keep
    # private$check_filtering()
    
    self$table_rsd_data <- rsd_data
    
    return(invisible(self))
  }
}


#' @title Show RSD histogram
#' 
#' @description
#' Show the histogram of RSD values.
#'
#' @param self object of class DataImport.
#'
#' @return ggplot2 object, RSD histogram.
#'
#' @importFrom ggplot2 ggplot aes geom_histogram .data theme_minimal labs
#'     geom_vline guide_legend theme .data
#'
qc_plot_rsd <- function(self = NULL) {
  if(!is.null(self$table_rsd_data)) {
    if(is.null(self$qc_rsd_limit)) {
      cli::cli_alert_info("No RSD limit set!")
    }
    
    p <- self$table_rsd_data |> 
      ggplot2::ggplot(ggplot2::aes(x = .data$rsd,
                                   fill = .data$polarity)) +
      ggplot2::geom_histogram(binwidth = 0.01,
                              alpha = 0.5) +
      ggplot2::geom_vline(xintercept = self$.qc_rsd_limit,
                          color = "red",
                          linetype = 2) +
      ggplot2::guides(fill = ggplot2::guide_legend(title = "Polarity",
                                                   override.aes = list(alpha = 1))) +
      ggplot2::labs(x = "RSD") +
      ggplot2::theme_minimal() +
      ggplot2::theme(legend.position = "bottom")
    
    return(p)
  } else {
    cli::cli_alert_danger("Can not create plot. No data available!")
    return(NULL)
  }
}


#' @title Calculate the trend of all features
#'
#' @description
#' Calculate the trend of all features.
#'
#' @param self object of class DataImport.
#'
#' @returns self (invisible).
#' 
qc_calc_trend = function(self = NULL) {
  if(!is.null(self$table_pool_long)) {
    id_col_meta <- self$id_col_meta
    meta_data <- self$table_metadata
    pools_data <- self$table_pool_long
    qcpool_index <- self$index_pools
    
    pools_data <- pools_data[pools_data[, "sampleName"] %in% qcpool_index, ]
    meta_data <- meta_data[meta_data[, id_col_meta] %in% qcpool_index, ]
    
    merge_data <- merge(
      x = pools_data,
      y = meta_data,
      by.x = "sampleName",
      by.y = id_col_meta
    )
    
    ref_data <- merge_data[merge_data[, self$order_column] == min(merge_data[, self$order_column]), c("id", "peakArea")]
    colnames(ref_data)[2] <- "refPeakArea"
    
    merge_data <- merge(
      x = merge_data,
      y = ref_data,
      by = "id"
    )
    
    merge_data$log2fc <- log2(merge_data$peakArea / merge_data$refPeakArea)
    merge_data$polarity <- gsub(pattern = "(pos|neg).*",
                                replacement = "\\1",
                                x = merge_data$id)
    
    self$table_trend_data <- merge_data
    
    return(invisible(self))
  }
}


#' @title Show the trend plot
#'
#' @description
#' Show the trend plot.
#'
#' @param self object of class DataImport.
#'
#' @return ggplot2 object, trend plot.
#'
#' @importFrom ggplot2 ggplot aes geom_line .data theme_minimal labs
#'     guide_legend theme geom_hline .data
#'
qc_plot_trend = function(self = NULL) {
  if(!is.null(self$table_trend_data)) {
    p <- self$table_trend_data |> 
      ggplot2::ggplot(ggplot2::aes(x = .data$sampleName,
                                   y = .data$log2fc,
                                   group = .data$id,
                                   colour = .data$polarity)) +
      ggplot2::geom_hline(yintercept = c(-1, 0, 1),
                          colour = c("black", "grey", "black"),
                          linetype = c(2, 1, 2)) +
      ggplot2::geom_line(alpha = 0.5) +
      ggplot2::guides(colour = ggplot2::guide_legend(title = "Polarity",
                                                     override.aes = list(alpha = 1))) +
      ggplot2::labs(x = "Sample id",
                    y = "Log2(fold change)") +
      ggplot2::theme_minimal() +
      ggplot2::theme(legend.position = "bottom",
                     axis.text.x = ggplot2::element_text(angle = 45,
                                                         hjust = 1))
    
    return(p)
  } else {
    cli::cli_alert_danger("Can not create plot. No data available!")
    return(NULL)
  }
}
