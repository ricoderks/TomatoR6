dev <- FALSE
if(dev) {
  library(TomatoR6)
  
  ##################################################### targeted lipidomics ####
  # create data import object
  obj3 <- MultiQuant$new(name = "Testing MultiQuant data")
  
  # set files
  obj3$file_data <- c(
    "/home/ricoderks/Downloads/TomatoR6_data/MQ_data.txt"
  )
  obj3$file_meta <- "/home/ricoderks/Downloads/TomatoR6_data/MQ_metadata.xlsx"
  obj3$file_curation <- "/home/ricoderks/Downloads/TomatoR6_data/MQ_curation.xlsx"
  
  # set regex's
  # obj3$regex_blanks <- "blank"
  obj3$regex_pools <- "QC"
  obj3$regex_qcs <- "qcplasma"
  obj3$regex_samples <- "sample"
  obj3$regex_standards <- "IS"
  
  # set columns
  obj3$id_col_meta <- "sampleName"
  obj3$order_column <- "measuring_order"
  obj3$type_column <- "sampleType"
  obj3$group_column <- "group"
  obj3$batch_column <- "batch_no"
  
  obj3$import()
  
  
  
  
  
  
  
}  
