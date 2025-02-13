#---------------------------------------------- Untargeted lipidomics class ----
#' @title R6 untargeted lipidomics class
#' 
#' @description
#' Untargeted lipidomics class. This is a child class from DataImport class.
#' 
#' @import R6
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
    }
  ), # end public
  private = list(
    import_data = function() {
      import_read_rawdata(self = self)
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
      extract_feature_data(self = self)
    }
  )
)