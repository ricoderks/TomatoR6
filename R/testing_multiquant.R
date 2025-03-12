dev <- FALSE
if(dev) {
  library(TomatoR6)
  
  ##################################################### targeted lipidomics ####
  # create data import object
  obj3 <- MultiQuant$new(name = "Testing MultiQuant data")
  
  # set files
  obj3$file_data <- c(
    "/home/rjederks/Downloads/TomatoR6_data/Batch1_output_merge.xlsx",
    "/home/rjederks/Downloads/TomatoR6_data/Batch2_output_merge.xlsx"
  )
  obj3$file_meta <- "/home/rjederks/Downloads/TomatoR6_data/metadata_lipidyzer.xlsx"
  
}  
