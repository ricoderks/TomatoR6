dev <- FALSE
if(dev) {
  library(TomatoR6)
  
  ##################################################### targeted lipidomics ####
  # create data import object
  obj2 <- TargetedLipidomics$new(name = "Testing targeted lipidomics")
  
  # set files
  obj2$file_data <- c(
    "/home/rjederks/Downloads/TomatoR6_data/Batch1_output_merge.xlsx",
    "/home/rjederks/Downloads/TomatoR6_data/Batch2_output_merge.xlsx"
  )
  obj2$file_meta <- "/home/rjederks/Downloads/TomatoR6_data/metadata_lipidyzer.xlsx"
  
  # obj2$lipidyzer_sheet <- 1
  
  # set regex's
  obj2$regex_blanks <- "blank"
  obj2$regex_pools <- "qcc"
  obj2$regex_samples <- "sample"
  
  # set columns
  obj2$id_col_meta <- "sampleId"
  obj2$order_column <- "injOrder"
  obj2$type_column <- "sampleType"
  obj2$group_column <- "group"
  obj2$batch_column <- "batch"
  
  # set preprocessing steps
  obj2$preprocessing_steps <- c("rsd_filter", "blank_filter", "total_normalisation")
  
  # set params
  # rsd
  obj2$qc_rsd_limit <- 0.3
  # blank
  obj2$blank_ratio <- 5
  obj2$blank_threshold <- 0.8
  obj2$blank_group_threshold <- 0.8
  
  obj2$import()  
  obj2$extract_additional_tables()
  
  ## View QC stuff
  # calculate QC stuff
  obj2$calc_qc()
  
  # plot QC stuff
  obj2$plot_qc_rsd()
  obj2$plot_qc_class_rsd()
  obj2$plot_qc_trend()
  obj2$plot_qc_cor()
  
  # do pre-processing
  obj2$preprocessing()
  obj2$plot_qc_rsd(type = "filtered")
  obj2$plot_qc_class_rsd(type = "filtered")
  obj2$plot_qc_trend(type = "filtered")
  
  # show qc now with preprocessed data
  obj2$calc_qc()
  obj2$plot_qc_rsd()
  obj2$plot_qc_class_rsd()
  obj2$plot_qc_trend()
  obj2$plot_qc_cor()
  
  
}  
