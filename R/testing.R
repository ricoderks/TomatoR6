dev <- FALSE
if(dev) {
  # library(TomatoR6)
  
  # create object
  data_obj <- UntargetedLipidomics$new(name = "Testing")
  
  # set file names
  data_obj$set_files(
    data_files = c(
      "/home/rjederks/Downloads/TomatoR6_data/20250122_100040_bile_pos_fixed.txt",
      "/home/rjederks/Downloads/TomatoR6_data/20250123_102728_bile_neg_fixed.txt"
    ),
    meta_file = "/home/rjederks/Downloads/TomatoR6_data/meta_data.xlsx"
  )
  
  # set several parameters
  data_obj$set_parameters(
    ids = list(
      id_col_meta = "sampleId",
      id_col_data = "sampleName"
    ),
    columns = list(
      type_column = "sampleTpe",
      group_column = "group",
      batch_column = "batch",
      order_column = "injOrder"
    ),
    regex = list(
      blanks = "blank_",
      pools = "qcpool_",
      samples = "sample_"
    ),
    rsd = list(
      rsd_limit = 0.3
    )
  )
  
  # import all data
  data_obj$import()
  
  # calculate all qcpool things
  data_obj$calc_qcpool()
  
  data_obj
  
  # show the info
  data_obj$show_data_info()
  
  # plots
  data_obj$plot_qc_rsd()
  data_obj$plot_qc_rsd_class()
  data_obj$plot_qc_trend()
  
  # history
  data_obj$history

  
  
  
    
}