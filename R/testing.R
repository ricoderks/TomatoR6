dev <- FALSE
if(dev) {
  library(TomatoR6)
  
  # create data import object
  untar_lipids <- UntargetedLipidomics$new(name = "Testing untargeted lipidomics")
  untar_lipids
  
  untar_lipids$file_data <- c(
    "/home/ricoderks/Downloads/TomatoR6_data/20250123_124538_bile_pos_fixed.txt",
    "/home/ricoderks/Downloads/TomatoR6_data/20250123_140224_bile_neg_fixed.txt"
  )
  untar_lipids$file_meta <- "/home/ricoderks/Downloads/TomatoR6_data/metadata.xlsx"
  untar_lipids$file_data
  
  untar_lipids$history  
  
  untar_lipids
  
  untar_lipids$import()
}  
