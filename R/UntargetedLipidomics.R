#' Defining the Untargeted lipidomics class
#'
#' @import R6
#'
#' @author Rico Derks
#' @author Yassene Mohammed
#' 
#' @export
#' 
#' @name UntargetedLipidomics
NULL

UntargetedLipidomics <- R6::R6Class(
  inherit = DataImport,
  classname = "UntargetedLipidomics",
  #----------------------------------------------------------------- public ----
  public = list(
    initialize = function(name = NA) {
      super$initialize(name)
    },
    plot_qc_class_rsd = function(type = NULL) {
      qc_plot_class_rsd(self = self,
                        type = type)
    }
  ), # end public
  #---------------------------------------------------------------- private ----
  private = list(
    import_data = function() {
      import_read_msdial(self = self)
      private$extract_featuredata()
      private$make_data_long()
      private$make_data_wide()
    },
    make_data_long = function() {
      make_table_long_msdial(self = self)
    },
    make_data_wide = function() {
      make_table_wide(self = self)
    },
    extract_featuredata = function() {
      extract_lipid_data_msdial(self = self)
    }
  ) # end private
)