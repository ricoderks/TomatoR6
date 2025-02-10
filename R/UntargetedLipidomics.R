#---------------------------------------------- Untargeted lipidomics class ----
#' @title R6 untargeted lipidomics class
#' 
#' @description
#' Untargeted lipidomics class. This is a child class from DataImport class.
#' 
#' @field msdial_files list(), containing all file names / locations.
#' 
#' @import R6
#' @importFrom tidyr pivot_wider pivot_longer
#' 
#' @export
#' 
#' @author Rico Derks
#' @author Yassene Mohammed
#'
UntargetedLipidomics <- R6::R6Class(
  inherit = DataImport,
  classname = "UntargetedLipidomics",
  public = list(
    #' @description
    #' Initialization function for DataImport class.
    #' 
    #' @param name character(1), name.
    #' 
    initialize = function(name = NA) {
      super$initialize(name)
      private$add_log(message = "Created untargeted lipidomics object")
      
      cli::cli_h2("Untargeted lipidomics")
      cli::cli_text("Untarageted lipidomics object created.")
      cli::cli_rule()
      cli::cli_text("Please set the following with `set_files()`:")
      files <- cli::cli_ul()
      cli::cli_li("Data file names")
      cli::cli_li("Meta data files")
      cli::cli_end(files)
      cli::cli_rule()
      cli::cli_text("Please set the following with `set_parameters()`:")
      params <- cli::cli_ul()
      cli::cli_li("ID columns for data and meta data")
      cli::cli_li("Column names, i.e. sample type, group, etc.")
      cli::cli_li("Regular expression, to recognize the diifferent samples.")
      cli::cli_end(params)
      cli::cli_rule()
      cli::cli_text("Next step: run `import()` to import all data.")
    },
    
    
    #---------------------------------------------------------------- files ----
    msdial_files = list(
      data_folder = NULL,
      parameter_file = NULL
    ),
    
    #--------------------------------------------------------- file methods ----
    #' @description
    #' Set the file names for data and meta data.
    #' 
    #' @param data_files character(), containing the full file names.
    #' @param meta_file character(1), containing the full meta file name.
    #' @param msdial_folder character(1), containing the folder with all MS data.
    #' @param msdial_parameter character(1), name of MSDIAL parameter file.
    #' 
    set_files = function(data_files = NULL, 
                         meta_file = NULL, 
                         msdial_folder = NULL,
                         msdial_parameter = NULL) {
      cli::cli_h3("Setting files:")
      cli::cli_ul()
      if(!is.null(data_files)) {
        cli::cli_li("data files")
        self$files$data_files <- data_files
        private$add_log(message = "Set data file(s)")
      }
      if(!is.null(meta_file)) {
        cli::cli_li("meta data file")
        self$files$meta_filename <- meta_file
        private$add_log(message = "Set meta data file")
      }
      if(!is.null(msdial_folder)) {
        cli::cli_li("MSDIAL folder")
        self$msdial_files$data_folder <- msdial_folder
        private$add_log(message = "Set MSDIAL folder")
      }
      if(!is.null(msdial_parameter)) {
        cli::cli_li("MSDIAL parameter file")
        self$msdial_files$parameter_file <- msdial_parameter
        private$add_log(message = "MSDIAL parameter file")
      }
      cli::cli_end()
      cli::cli_alert_success("Done!")
    },
    
    
    #---------------------------------------------------------- global info ----    
    
    
    #----------------------------------------------------------- run MSDIAL ----
    #' @description
    #' Running command line MSDIAL from R. MSDIAL needs to be installed to be 
    #' able to use this.
    #' 
    run_msdial = function() {
      TOMATO_MSDIAL_PATH <- Sys.getenv("TOMATO_MSDIAL_PATH")
      if(TOMATO_MSDIAL_PATH != "") {
        cat("MSDIAL path:", TOMATO_MSDIAL_PATH, "\n")
      }
    },
    
    
    #----------------------------------------------------- plotting methods ----
    #' @description
    #' A violin plot showing the RSD of each lipid species per lipid class.
    #' 
    #' @return ggplot2 object containing a violin plot.
    #' 
    plot_qc_rsd_class = function() {
      # Rico: This might also useful for the untargeted metabolomics. MSDIAL 
      # also gives a class (ontology) for identified metabolites if available.
      # Maybe even for other child classes as well. Needs to be place in parent 
      # class then.
    }
  ), # end public
  #---------------------------------------------------------------- private ----
  private = list(
    import_data = function() {
      data_df <- data.frame()
      
      for(a in 1:length(self$files$data_files)) {
        tmp <- private$read_msdial(file = self$files$data_files[a])
        
        data_df <- rbind.data.frame(data_df, tmp)
      }
      
      data_df <- private$cleanup(data_df = data_df)
      self$tables$raw_data <- data_df
      
      # get the features and information
      private$extract_feature_data(data_df = data_df)
      
      # convert raw data table
      private$make_table_long(data_wide = data_df)
      private$make_table_wide(data_long = self$tables$all_data_long)
    },
    
    read_msdial = function(file = NULL) {
      data_df <- utils::read.table(file = file,
                                   header = TRUE,
                                   sep = "\t",
                                   skip = 4,
                                   comment.char = "")
      
      return(data_df)
    },
    
    cleanup = function(data_df = NULL) {
      # add unique ID
      data_df$polarity <- ifelse(grepl(pattern = "\\+$",
                                       x = data_df$Adduct.type),
                                 "pos",
                                 "neg")
      
      data_df$id <- paste0(data_df$polarity, "_", data_df$Alignment.ID)
      
      # remove unknowns and others
      data_df <- data_df[!(grepl(pattern = "(unknown|no MS2|low score)",
                                 x = data_df$Metabolite.name,
                                 ignore.case = TRUE) |
                             grepl(pattern = "(unknown|others)",
                                   x = data_df$Ontology,
                                   ignore.case = TRUE)), ]
      
      return(data_df)
    },
    
    extract_feature_data = function(data_df = NULL) {
      data_df <- data_df[, c("id", "Metabolite.name", "Ontology", "Adduct.type", "Average.Rt.min.", "Average.Mz")]
      split_name <- strsplit(x = data_df$Metabolite.name,
                             split = "\\|")
      
      short <- vector(mode = "character",
                      length = length(split_name))
      long <- vector(mode = "character",
                     length = length(split_name))
      for(a in 1:length(split_name)) {
        if(length(split_name[[a]]) == 1) {
          short[a] <- split_name[[a]][1]
          long[a] <- split_name[[a]][1]
        } else {
          short[a] <- split_name[[a]][1]
          long[a] <- split_name[[a]][2]
        }
      }
      
      data_df$shortLipidName <- short
      data_df$longLipidname <- long
      # for now keep all features
      data_df$keep <- TRUE
      
      self$tables$feature_data <- data_df
    },
    
    #' @importFrom tidyr pivot_longer
    make_table_long = function(data_wide = NULL) {
      data_long <- data_wide[, c("id", colnames(data_wide)[grepl(pattern = "(blank|qcpool|sample)_",
                                                                 x = colnames(data_wide),
                                                                 ignore.case = TRUE)])] |> 
        tidyr::pivot_longer(
          cols = colnames(data_wide)[grepl(pattern = "(blank|qcpool|sample)_",
                                           x = colnames(data_wide),
                                           ignore.case = TRUE)],
          names_to = "sampleName",
          values_to = "peakArea"
        )
      
      self$tables$all_data_long <- as.data.frame(data_long)
    },
    
    make_table_wide = function(data_long = NULL) {
      data_wide <- data_long |> 
        tidyr::pivot_wider(
          id_cols = "sampleName",
          names_from = "id",
          values_from = "peakArea"
        )
      
      self$tables$all_data <- as.data.frame(data_wide)
    }
    
    
  ) # end private
)