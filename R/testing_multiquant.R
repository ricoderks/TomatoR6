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
  
  obj3$import()
  
  
  
  
  
  
  
}  
