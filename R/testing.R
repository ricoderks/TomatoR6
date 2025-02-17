dev <- FALSE
if(dev) {
  library(TomatoR6)
  
  # create data import object
  obj <- UntargetedLipidomics$new(name = "Testing untargeted lipidomics")
  
  # set files
  obj$file_data <- c(
    "/home/ricoderks/Downloads/TomatoR6_data/20250123_124538_bile_pos_fixed.txt",
    "/home/ricoderks/Downloads/TomatoR6_data/20250123_140224_bile_neg_fixed.txt"
  )
  obj$file_meta <- "/home/ricoderks/Downloads/TomatoR6_data/metadata.xlsx"
  
  # set regex's
  obj$regex_blanks <- "blank_"
  obj$regex_pools <- "qcpool_"
  obj$regex_samples <- "sample_"
  
  # set columns
  obj$id_col_meta <- "sampleId"
  obj$order_column <- "injOrder"
  obj$type_column <- "sampleType"
  obj$group_column <- "group"
  
  # set preprocessing steps
  obj$preprocessing_steps <- c("rsd_filter", "blank_filter")
  
  # set params
  # rsd
  obj$qc_rsd_limit <- 0.3
  # blank
  obj$blank_ratio <- 5
  obj$blank_threshold <- 0.8
  obj$blank_group_threshold <- 0.8
  
  # Import all data
  obj$import()
  
  
  ## View QC stuff
  # calculate QC stuff
  obj$calc_qc()
  
  # plot QC stuff
  obj$plot_qc_rsd()
  obj$plot_qc_class_rsd()
  obj$plot_qc_trend()
  obj$plot_qc_cor()
  
  
  
  # do pre-processing
  obj$preprocessing()
  obj$plot_qc_rsd(type = "filtered")
  obj$plot_qc_class_rsd(type = "filtered")
  obj$plot_qc_trend(type = "filtered")
  
  
  
  
  
}  
