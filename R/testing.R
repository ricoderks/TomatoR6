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
  
  # set regex's
  untar_lipids$regex_blanks <- "blank_"
  untar_lipids$regex_pools <- "qcpool_"
  untar_lipids$regex_samples <- "sample_"
  
  # set columns
  untar_lipids$order_column <- "injOrder"
  untar_lipids$type_column <- "sampleType"
  untar_lipids$group_column <- "group"
  
  # set params
  untar_lipids$qc_rsd_limit <- 0.3
  
  untar_lipids$import()
  
  # calculate QC stuff
  untar_lipids$calc_qc()
  
  # plot QC stuff
  untar_lipids$plot_qc_rsd()
  untar_lipids$plot_qc_trend()
  

  
  
  
  
  
}  
