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
#' @noRd
#'
#' @returns self (invisible).
#'
qc_calc_rsd <- function(self = NULL,
                        private = NULL) {
  if(!is.null(self$table_analysis_long)) {
    feature_data <- self$table_featuredata
    pools_data <- self$table_analysis_long
    qcpool_index <- self$index_pools
    
    pools_data <- pools_data[pools_data[, "sampleId"] %in% qcpool_index, ]
    
    rsd_data <- tapply(pools_data, list(pools_data[, "featureId"]), function(x) {
      rsd <- stats::sd(x[, "value"], na.rm = TRUE) / mean(x[, "value"], na.rm = TRUE)
      return(data.frame("featureId" = x[1, "featureId"],
                        "rsd" = rsd))
    })
    rsd_data <- do.call("rbind", rsd_data)
    
    rsd_data <- merge(
      x = rsd_data,
      y = feature_data[, c("featureId", "class", "polarity")],
      by.x = "featureId",
      by.y = "featureId"
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
#' @noRd
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
      self$table_rsd_data$featureId %in% self$table_featuredata$featureId[self$table_featuredata$keep], 
    ]
    } else {
      plot_data <- self$table_rsd_data
    }
    
    p <- plot_data |> 
      ggplot2::ggplot(ggplot2::aes(x = .data$rsd,
                                   fill = .data$polarity)) +
      ggplot2::geom_histogram(binwidth = 0.01,
                              alpha = 0.5) +
      ggplot2::geom_vline(xintercept = self$qc_rsd_limit,
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
#' @noRd
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
        self$table_rsd_data$featureId %in% self$table_featuredata$featureId[self$table_featuredata$keep], 
      ]
    } else {
      plot_data <- self$table_rsd_data
    }
    
    p <- plot_data |> 
      ggplot2::ggplot(ggplot2::aes(x = .data$class,
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
#' @noRd
#'
#' @returns self (invisible).
#' 
qc_calc_trend = function(self = NULL) {
  if(!is.null(self$table_analysis_long)) {
    feature_data <- self$table_featuredata
    id_col_meta <- self$id_col_meta
    meta_data <- self$table_metadata
    pools_data <- self$table_analysis_long
    qcpool_index <- self$index_pools
    
    pools_data <- pools_data[pools_data[, "sampleId"] %in% qcpool_index, ]
    meta_data <- meta_data[meta_data[, id_col_meta] %in% qcpool_index, ]
    
    merge_data <- merge(
      x = pools_data,
      y = meta_data,
      by.x = "sampleId",
      by.y = id_col_meta
    )
    
    # order the QC samples in measurment order for later plotting with ggplot2
    unique_qc <- unique(merge_data$sampleId)
    unique_order <- unique(merge_data[, self$order_column])
    
    merge_data$sampleId <- factor(x = merge_data$sampleId,
                                    levels = unique_qc[order(unique_order)],
                                    labels = unique_qc[order(unique_order)])
    
    ref_data <- merge_data[merge_data[, self$order_column] == min(merge_data[, self$order_column]), c("featureId", "value")]
    colnames(ref_data)[2] <- "refValue"
    
    merge_data <- merge(
      x = merge_data,
      y = ref_data,
      by = "featureId"
    )
    
    merge_data$log2fc <- log2(merge_data$value / merge_data$refValue)
    
    merge_data <- merge(
      x = merge_data,
      y = feature_data[, c("featureId", "polarity")],
      by.x = "featureId",
      by.y = "featureId"
    )
    
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
#' @noRd
#'
#' @importFrom ggplot2 ggplot aes geom_line .data theme_minimal labs
#'     guide_legend theme geom_hline .data facet_wrap vars geom_point
#'
qc_plot_trend = function(self = NULL,
                         type = c("all", "filtered")) {
  type <- match.arg(arg = type,
                    choices = c("all", "filtered"))
  
  if(!is.null(self$table_trend_data)) {
    if(type == "filtered") {
      plot_data <- self$table_trend_data[
        self$table_trend_data$featureId %in% self$table_featuredata$featureId[self$table_featuredata$keep], 
      ]
    } else {
      plot_data <- self$table_trend_data
    }
    
    p <- plot_data |> 
      ggplot2::ggplot(ggplot2::aes(x = .data$sampleId,
                                   y = .data$log2fc,
                                   group = .data$featureId)) +
      ggplot2::geom_hline(yintercept = -1,
                          linetype = 2,
                          colour = "black") +
      ggplot2::geom_hline(yintercept = 1,
                          linetype = 2,
                          colour = "black") +
      ggplot2::geom_hline(yintercept = 0,
                          linetype = 1,
                          colour = "grey")
      if(is.null(self$batch_column)) {
        p <- p +
          ggplot2::geom_line(ggplot2::aes(colour = .data$polarity),
                             alpha = 0.3) +
          ggplot2::geom_point(ggplot2::aes(colour = .data$polarity)) +
          ggplot2::guides(colour = ggplot2::guide_legend(title = "Polarity",
                                                         override.aes = list(alpha = 1)))
      } else {
        p <- p +
          ggplot2::geom_line(ggplot2::aes(colour = as.factor(.data[[self$batch_column]])),
                             alpha = 0.3) +
          ggplot2::geom_point(ggplot2::aes(colour = as.factor(.data[[self$batch_column]]))) +
          ggplot2::guides(colour = ggplot2::guide_legend(title = "Batch",
                                                         override.aes = list(alpha = 1)))
      }
    p <- p +
      ggplot2::labs(x = "Sample id",
                    y = "Log2(fold change)") +
      ggplot2::facet_wrap(ggplot2::vars(.data$polarity),
                          nrow = 2,
                          scales = "free_y") +
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
#' @noRd
#' 
#' @returns self (invisible).
#' 
qc_apply_rsd <- function(self = NULL) {
  rsd_data <- self$table_rsd_data
  rsd_limit <- self$qc_rsd_limit
  
  keep <- rsd_data$featureId[rsd_data$rsd <= rsd_limit]
  self$table_featuredata$keep_rsd <- self$table_featuredata$featureId %in% keep
  
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
#' @noRd
#' 
#' @returns self (invisible).
#' 
qc_calc_cor <- function(self = NULL) {
  if(!is.null(self$table_analysis)) {
    index_pools <- self$index_pools
    index_samples <- self$index_samples
    
    data_df <- self$table_analysis[self$table_analysis$sampleId %in% c(index_pools, index_samples), ]
    rownames(data_df) <- data_df$sampleId
    data_df <- t(data_df[, -1])
    
    data_df[data_df == 0] <- 1
    data_df[is.na(data_df)] <- 1
    
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
#' @noRd
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


#' @title Calculate the area ratio normalized to the mean
#' 
#' @description
#' Calculate the area ratio normalized to the mean.
#' 
#' @param self object of class DataImport.
#' 
#' @noRd
#' 
#' @returns self (invisible).
#' 
qc_calc_norm_arearatio <- function(self = NULL) {
  data_df <- self$table_analysis
  data_long_df <- self$table_analysis_long
  meta_data <- self$table_metadata
  idx_qcs <- self$index_qcs
  idx_pools <- self$index_pools
  
  idx_all <- c(idx_qcs, idx_pools)
  
  norm_data <- data_df[data_df$sampleId %in% idx_all, ]
  norm_data[, -1] <- t(t(norm_data[, -1]) / colMeans(norm_data[, -1], na.rm = TRUE))
  
  norm_data_long <- utils_make_analysis_long(df = norm_data)
  norm_data_long <- merge(
    x = norm_data_long,
    y = meta_data,
    by = "sampleId"
  )
  
  self$table_norm_arearatio <- norm_data
  self$table_norm_arearatio_long <- norm_data_long
  
  return(invisible(self))
}


#' @title Create normalized area ratio plot
#' 
#' @description
#' Create normalized area ratio plot.
#' 
#' @param self class object.
#' 
#' @returns normalized area ratio plot (ggplot2 object).
#' 
#' @importFrom ggplot2 aes .data geom_jitter scale_x_continuous facet_grid vars 
#'     labs theme element_blank element_text
#' 
#' @noRd
#' 
plot_norm_arearatio <- function(self = NULL) {
  p <- self$table_norm_arearatio_long |> 
    ggplot2::ggplot(ggplot2::aes(x = .data$value, 
                                 y = .data[[self$type_column]], 
                                 color = .data[[self$type_column]])) +
    ggplot2::geom_jitter(height = 0.3, 
                         width = 0) +
    ggplot2::scale_x_continuous(trans = "log10") +
    ggplot2::facet_grid(rows = ggplot2::vars(.data$featureId)) +
    ggplot2::labs(title = "Comparison signal over samples",
                  subtitle = "area ratio normalized to average") +
    ggplot2::theme(axis.text.y = ggplot2::element_blank(),
                   axis.title.y = ggplot2::element_blank(),
                   strip.text.y = ggplot2::element_text(angle = 0))
  
  return(p)
}