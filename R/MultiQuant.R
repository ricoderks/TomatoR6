#' Defining the Sciex Multi Quant class
#'
#' @import R6
#'
#' @author Rico Derks
#' @author Yassene Mohammed
#' @author Marieke Heijink
#' 
#' @export
#' 
#' @name MultiQuant
NULL

MultiQuant <- R6::R6Class(
  inherit = DataImport,
  classname = "MultiQuant",
  active = list(
    file_curation = function(value) {
      if(missing(value)) {
        self$.file_curation
      } else {
        self$.file_curation <- value
        private$add_log(message = paste0("Added curation file: '", 
                                         self$.file_curation,
                                         "'"))
      }
    }
  ),
  public = list(
    initialize = function(name = NA) {
      super$initialize(name)
    },
    #---------------------------------------------------------------- files ----
    .file_curation = NULL
  ),
  private = list(
    #---------------------------------------------------------------- print ----
    file_show = function() {
      utils_file_show_mq(self = self)
    },
    import_data = function() {
      import_multiquant(self = self)
      # private$extract_featuredata()
      # private$make_data_long()
      # private$make_data_wide()
    }
  )
)