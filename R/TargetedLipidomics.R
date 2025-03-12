#' Defining the Targeted lipidomics class
#'
#' @import R6
#'
#' @author Rico Derks
#' @author Yassene Mohammed
#' 
#' @export
#' 
#' @name TargetedLipidomics
NULL

TargetedLipidomics <- R6::R6Class(
  inherit = DataImport,
  classname = "TargetedLipidomics",
  active = list(
    lipidyzer_sheet = function(value) {
      if(missing(value)) {
        private$.lipidyzer_sheet
      } else {
        private$.lipidyzer_sheet <- value
        private$add_log(message = paste0("Set lipidyzer sheet : '", 
                                         private$.lipidyzer_sheet, 
                                         "'"))
      }
    },
    split_PE = function(value) {
      if(missing(value)) {
        private$.split_PE
      } else {
        private$.split_PE <- value
        private$add_log(message = paste0("Set split PE class : '", 
                                         private$.split_PE, 
                                         "'"))
      }
    }
  ),
  #----------------------------------------------------------------- public ----
  public = list(
    initialize = function(name = NA) {
      super$initialize(name)
      # default settings
      self$lipidyzer_sheet <- 1
      self$split_PE <- FALSE
    },
    plot_qc_class_rsd = function(type = NULL) {
      qc_plot_class_rsd(self = self,
                        type = type)
    },
    extract_additional_tables = function() {
      cli::cli_h3("Extracting additional tables")
      utils_extract_additional_tables(self = self)
      private$add_log(message = "Extracted additional tables.")
      cli::cli_alert_success("Done!")
    },
    #---------------------------------------------------------------- tables ---
    # wide data
    table_species_comp = NULL,
    table_class_conc = NULL,
    table_class_comp = NULL,
    
    # long data
    table_species_comp_long = NULL,
    table_class_conc_long = NULL,
    table_class_comp_long = NULL
  ), # end public
  #---------------------------------------------------------------- private ----
  private = list(
    .lipidyzer_sheet = NULL,
    .split_PE = NULL,
    import_data = function() {
      import_lipidyzer(self = self)
      private$extract_featuredata()
      private$make_data_long()
      private$make_data_wide()
    },
    make_data_long = function() {
      make_table_long_lipidyzer(self = self)
    },
    make_data_wide = function() {
      make_table_wide(self = self)
    },
    extract_featuredata = function() {
      extract_lipid_data_lipidyzer(self = self,
                                   private = private)
    },
    lipid_class_pos = c("SM", "TG", "CE", "DG", "Cer d18:1", "Cer d18:0", 
                         "HexCER d18:1", "LacCER d18:1", "PA", "LPC", "LPE"),
    lipid_class_neg = c("FA", "PC", "PE", "P-PE", "PG", "PI", "PS")
  ) # end private
)