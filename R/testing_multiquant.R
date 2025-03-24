dev <- FALSE
if(dev) {
  library(TomatoR6)
  
  ##################################################### targeted lipidomics ####
  # create data import object
  obj3 <- MultiQuant$new(name = "Testing MultiQuant data")
  
  # set files
  obj3$file_data <- c(
    "/home/rjederks/Downloads/TomatoR6_data/sciex_mq_example_4_noIS.txt"
  )
  obj3$file_meta <- "/home/rjederks/Downloads/TomatoR6_data/sciex_mq_example_4_noIS_metadata.xlsx"
  obj3$file_curation <- "/home/rjederks/Downloads/TomatoR6_data/sciex_mq_example_4_noIS_notes-integration.xlsx"
  
  # set regex's
  # obj3$regex_blanks <- "blank"
  obj3$regex_pools <- "qcpool"
  obj3$regex_qcs <- "^QC$"
  obj3$regex_samples <- "sample"
  
  # set columns
  # these two will be overriden when obj3$extract_metadata is set to TRUE
  # obj3$id_col_meta <- "sampleId"
  # obj3$order_column <- "injOrder"
  obj3$data_to_extract <- "Area_Ratio"
  obj3$type_column <- "sampleType"
  obj3$group_column <- "blocks"
  obj3$batch_column <- "batch"
  
  obj3$extract_metadata <- TRUE
  
  # set params
  # rsd
  obj3$qc_rsd_limit <- 0.3
  
  # blank
  obj3$blank_ratio <- 5
  obj3$blank_threshold <- 0.8
  obj3$blank_group_threshold <- 0.8
  
  # set preprocessing steps
  obj3$preprocessing_steps <- c("rsd_filter")
  
  
  #----------
  obj3$import()
  
  obj3$calc_qc()

  obj3$plot_qc_rsd()
  obj3$plot_qc_cor()
  obj3$plot_qc_trend()
  obj3$plot
  
  obj3$preprocessing()
  
  
}  
