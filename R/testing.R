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
  
  untar_lipids$id_col_meta <- "sampleId"
  
  untar_lipids$regex_blanks <- "blank_"
  untar_lipids$regex_pools <- "qcpool_"
  untar_lipids$regex_samples <- "sample_"
  
  untar_lipids$import()
  
  untar_lipids$history  
  
  untar_lipids
  
}  
