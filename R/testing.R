dev <- FALSE
if(dev) {
  library(TomatoR6)
  
  ################################################### untargeted lipidomics ####
  # create data import object
  obj1 <- UntargetedLipidomics$new(name = "Testing untargeted lipidomics")
  
  # set files
  obj1$file_data <- c(
    "/home/ricoderks/Downloads/TomatoR6_data/20250123_124538_pos.txt",
    "/home/ricoderks/Downloads/TomatoR6_data/20250123_140224_neg.txt"
  )
  obj1$file_meta <- "/home/ricoderks/Downloads/TomatoR6_data/metadata.xlsx"
  
  # set regex's
  obj1$regex_blanks <- "blank"
  obj1$regex_pools <- "qcpool"
  obj1$regex_samples <- "sample"
  
  # set columns
  obj1$id_col_meta <- "sampleId"
  obj1$order_column <- "injOrder"
  obj1$type_column <- "sampleType"
  obj1$group_column <- "group"
  obj1$batch_column <- "batch"
  
  # set preprocessing steps
  obj1$preprocessing_steps <- c("rsd_filter", "blank_filter")
  
  # set params
  # rsd
  obj1$qc_rsd_limit <- 0.3
  # blank
  obj1$blank_ratio <- 5
  obj1$blank_threshold <- 0.8
  obj1$blank_group_threshold <- 0.8
  # normalisation
  # default is median
  obj1$norm_pqn_reference <- "median"
  # imputation
  obj1$imp_method <- "min"
  # batch correction
  obj1$bc_method <- "loess"
  obj1$bc_loess_method <- "batch"
  obj1$bc_loess_span <- 0.75
  
  
  # Import all data
  obj1$import()
  
  
  ## View QC stuff
  # calculate QC stuff
  obj1$calc_qc()
  
  # plot QC stuff
  obj1$plot_qc_rsd()
  obj1$plot_qc_class_rsd()
  obj1$plot_qc_trend()
  obj1$plot_qc_cor()
  
  
  
  # do pre-processing
  obj1$preprocessing()
  obj1$plot_qc_rsd(type = "filtered")
  obj1$plot_qc_class_rsd(type = "filtered")
  obj1$plot_qc_trend(type = "filtered")
  
  ##################################################### targeted lipidomics ####
  # create data import object
  obj2 <- TargetedLipidomics$new(name = "Testing targeted lipidomics")
  
  # set files
  obj2$file_data <- c(
    "/home/ricoderks/Downloads/TomatoR6_data/Batch1_output_merge.xlsx",
    "/home/ricoderks/Downloads/TomatoR6_data/Batch2_output_merge.xlsx"
  )
  obj2$file_meta <- "/home/ricoderks/Downloads/TomatoR6_data/metadata_lipidyzer.xlsx"
  
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
