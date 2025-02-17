dev <- FALSE
if(dev) {
  library(TomatoR6)
  
  # create data import object
  obj <- UntargetedLipidomics$new(name = "Testing untargeted lipidomics")
  obj
  
  obj$file_data <- c(
    "/home/ricoderks/Downloads/TomatoR6_data/20250123_124538_bile_pos_fixed.txt",
    "/home/ricoderks/Downloads/TomatoR6_data/20250123_140224_bile_neg_fixed.txt"
  )
  obj$file_meta <- "/home/ricoderks/Downloads/TomatoR6_data/metadata.xlsx"
  
  obj$id_col_meta <- "sampleId"
  
  # set regex's
  obj$regex_blanks <- "blank_"
  obj$regex_pools <- "qcpool_"
  obj$regex_samples <- "sample_"
  
  # set columns
  obj$order_column <- "injOrder"
  obj$type_column <- "sampleType"
  obj$group_column <- "group"
  
  # set preprocessing steps
  obj$preprocessing_steps <- c("rsd_filter")
  
  # set params
  obj$qc_rsd_limit <- 0.3
  
  obj$import()
  
  # calculate QC stuff
  obj$calc_qc()
  
  # plot QC stuff
  obj$plot_qc_rsd()
  obj$plot_qc_trend()
  
  # do pre-processing
  obj$preprocessing()
  
  
  
  
  
}  
