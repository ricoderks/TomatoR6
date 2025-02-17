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
#' @param type character(1), all or filtered data.
#'
#' @return ggplot2 object, RSD histogram.
#'
#' @importFrom ggplot2 ggplot aes geom_histogram .data theme_minimal labs
#'     geom_vline guide_legend theme .data
#'
qc_plot_rsd <- function(self = NULL,
                        type = c("all", "filtered")) {
  type <- match.arg(arg = type,
                    choices = c("all", "filtered"))
  
  if(!is.null(self$table_rsd_data)) {
    if(is.null(self$qc_rsd_limit)) {
      cli::cli_alert_info("No RSD limit set!")
    }
    
    if(type == "filtered") {
    plot_data <- self$table_rsd_data[
      self$table_rsd_data$id %in% self$table_featuredata$id[self$table_featuredata$keep], 
    ]
    } else {
      plot_data <- self$table_rsd_data
    }
    
    p <- plot_data |> 
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


#' @title A violin plot showing the RSD of each lipid species per lipid class
#' 
#' @description
#' A violin plot showing the RSD of each lipid species per lipid class.
#'
#' @param self object of class DataImport.
#' @param type character(1), all or filtered data.
#'
#' @return ggplot2 object containing a violin plot.
#'
#' @importFrom ggplot2 ggplot aes geom_violin .data theme_minimal labs
#'     geom_hline guide_legend theme geom_jitter
#'
qc_plot_class_rsd <- function(self = NULL,
                              type = c("all", "filtered")) {
  type <- match.arg(arg = type,
                    choices = c("all", "filtered"))
  
  if(!is.null(self$table_rsd_data)) {
    if(is.null(self$qc_rsd_limit)) {
      cli::cli_alert_info("No RSD limit set!")
    }
    
    if(type == "filtered") {
      plot_data <- self$table_rsd_data[
        self$table_rsd_data$id %in% self$table_featuredata$id[self$table_featuredata$keep], 
      ]
    } else {
      plot_data <- self$table_rsd_data
    }
    
    p <- plot_data |> 
      ggplot2::ggplot(ggplot2::aes(x = .data$Ontology,
                                   y = .data$rsd)) +
      ggplot2::geom_violin() +
      ggplot2::geom_jitter(ggplot2::aes(colour = .data$polarity),
                           alpha = 0.7) +
      ggplot2::geom_hline(yintercept = self$qc_rsd_limit,
                          color = "red",
                          linetype = 2) +
      ggplot2::guides(colour = ggplot2::guide_legend(title = "Polarity",
                                                     override.aes = list(alpha = 1))) +
      ggplot2::labs(x = "Class",
                    y = "RSD") +
      ggplot2::theme_minimal() +
      ggplot2::theme(legend.position = "bottom",
                     axis.text.x = ggplot2::element_text(angle = 90,
                                                         hjust = 1))
    
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
#' @param type character(1), all or filtered data.
#'
#' @return ggplot2 object, trend plot.
#'
#' @importFrom ggplot2 ggplot aes geom_line .data theme_minimal labs
#'     guide_legend theme geom_hline .data
#'
qc_plot_trend = function(self = NULL,
                         type = c("all", "filtered")) {
  type <- match.arg(arg = type,
                    choices = c("all", "filtered"))
  
  if(!is.null(self$table_trend_data)) {
    if(type == "filtered") {
      plot_data <- self$table_trend_data[
        self$table_trend_data$id %in% self$table_featuredata$id[self$table_featuredata$keep], 
      ]
    } else {
      plot_data <- self$table_trend_data
    }
    
    p <- plot_data |> 
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


#' @title Apply the RSD filtering
#' 
#' @description
#' Apply the RSD filtering.
#' 
#' @param self object of class DataImport.
#' 
#' @returns self (invisible).
#' 
qc_apply_rsd <- function(self = NULL) {
  rsd_data <- self$table_rsd_data
  rsd_limit <- self$qc_rsd_limit
  
  keep <- rsd_data$id[rsd_data$rsd <= rsd_limit]
  self$table_featuredata$keep_rsd <- self$table_featuredata$id %in% keep
  
  return(invisible(self))
}


#' @title Calculate the correlation for all samples and qcpools
#' 
#' @description
#' Calculate the correlation for all samples and qcpools.
#' 
#' @param self object of class DataImport.
#' 
#' @importFrom stats cor
#' @importFrom tidyr pivot_longer
#' 
#' @returns self (invisible).
#' 
qc_calc_cor <- function(self = NULL) {
  if(!is.null(self$table_alldata)) {
    index_pools <- self$index_pools
    index_samples <- self$index_samples
    
    data_df <- self$table_alldata[self$table_alldata$sampleName %in% c(index_pools, index_samples), ]
    rownames(data_df) <- data_df$sampleName
    data_df <- t(data_df[, -1])
    
    data_df[data_df == 0] <- 1
    
    cor_df <- as.data.frame(stats::cor(log10(data_df)))
    cor_df$x <- rownames(cor_df)
    
    cor_df_long <- cor_df |> 
      tidyr::pivot_longer(colnames(cor_df)[-ncol(cor_df)],
                          names_to = "y",
                          values_to = "cor")
    
    self$table_cor_data <- cor_df_long
    
    return(invisible(self))
  }
}


#' @title Show correlation heatmap samples and qcpools
#' 
#' @description
#' Show correlation heatmap samples and qcpools.
#' 
#' @param self object of class DataImport.
#' 
#' @importFrom ggplot2 ggplot aes .data geom_tile scale_fill_gradient
#'     theme_minimal theme element_text element_blank guides guide_colourbar
#' 
#' @returns ggplot2, correlation heatmap.   
#' 
qc_plot_cor <- function(self = NULL) {
  if(!is.null(self$table_cor_data)) {
    p <- self$table_cor_data |>
      ggplot2::ggplot(ggplot2::aes(x = .data$x,
                                   y = .data$y)) +
      ggplot2::geom_tile(ggplot2::aes(fill = .data$cor),
                         color = "white",
                         lwd = 0.1,
                         linetype = 1) +
      ggplot2::scale_fill_gradient(limits = c(-1, 1),
                                   low = "blue",
                                   high = "red") +
      ggplot2::guides(fill = ggplot2::guide_colourbar(title = "Pearson corr.")) +
      ggplot2::theme_minimal() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90,
                                                         hjust = 1),
                     axis.title = ggplot2::element_blank())
    
    
    return(p)
  }
}



