#' Defining the Untargeted metabolomics class
#'
#' @import R6
#'
#' @author Rico Derks
#' @author Yassene Mohammed
#' 
#' @export
#' 
#' @name UntargetedMetabolomics
NULL

UntargetedMetabolomics <- R6::R6Class(
  inherit = DataImport,
  classname = "UntargetedMetabolomics",
  #----------------------------------------------------------------- PUBLIC ----
  public = list(
    initialize = function(name = NA) {
      super$initialize(name)
    }
  ), # end public
  #---------------------------------------------------------------- PRIVATE ----
  private = list(
    import_data = function() {
      import_read_msdial(self = self)
      private$extract_featuredata()
      private$make_data_long()
      private$make_data_wide()
    },
    make_data_long = function() {
      make_table_long(self = self)
    },
    make_data_wide = function() {
      make_table_wide(self = self)
    },
    extract_featuredata = function() {
      extract_metabolite_data(self = self)
    }
  ) # end private
)