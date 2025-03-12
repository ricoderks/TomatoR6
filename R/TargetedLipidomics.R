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
        self$.lipidyzer_sheet
      } else {
        self$.lipidyzer_sheet <- value
        private$add_log(message = paste0("Set lipidyzer sheet : '", 
                                         self$.lipidyzer_sheet, 
                                         "'"))
      }
    }
  ),
  #----------------------------------------------------------------- public ----
  public = list(
    initialize = function(name = NA) {
      super$initialize(name)
      self$lipidyzer_sheet <- 1
    },
    .lipidyzer_sheet = NULL,
    plot_qc_class_rsd = function(type = NULL) {
      qc_plot_class_rsd(self = self,
                        type = type)
    }
  ), # end public
  #---------------------------------------------------------------- private ----
  private = list(
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