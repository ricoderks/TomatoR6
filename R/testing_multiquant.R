dev <- FALSE
if(dev) {
  library(TomatoR6)
  
  ##################################################### targeted lipidomics ####
  # create data import object
  obj3 <- MultiQuant$new(name = "Testing MultiQuant data")
  
  # set files
  obj3$file_data <- c(
    "/home/rjederks/Downloads/TomatoR6_data/MQ_data.txt"
  )
  obj3$file_meta <- "/home/rjederks/Downloads/TomatoR6_data/MQ_metadata.xlsx"
  obj3$file_curation <- "/home/rjederks/Downloads/TomatoR6_data/MQ_curation.xlsx"
  
  # set regex's
  # obj3$regex_blanks <- "blank"
  obj3$regex_pools <- "Quality Control"
  obj3$regex_samples <- "Unknown"
  obj3$regex_standards <- "Standard"
  
  # set columns
  obj3$id_col_meta <- "cpm_code"
  obj3$order_column <- "measuring_order"
  # obj3$type_column <- "sampleType"
  obj3$group_column <- "group"
  obj3$batch_column <- "batch_no"
  
  
  
  obj3$import()
  
  
  
  
  
  
  
}  
