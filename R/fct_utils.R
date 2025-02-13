#' @title Add a log entry
#' 
#' @description
#' Add log entry into the history.
#' 
#' @param self self 
#' @param message character(1), log message.
#'
#' @returns self
#' 
utils_add_log <- function(self = NULL,
                          message = NULL) {
  if(is.null(self$history$id)) {
    id <- 0
  } else {
    id <- self$history$id[nrow(self$history)] + 1
  }
  self$history <- rbind.data.frame(
    self$history,
    data.frame(id = id,
               datetime = format(Sys.time(), "%Y-%m-%d %H:%M:%OS3"),
               message = message)
  )
  invisible(self)
}


#' @title Show the files from DataImport class
#' 
#' @description
#' Show the files from DataImport class.
#' 
#' @param self self 
#'
#' @import cli
#'
utils_file_show <-  function(self = NULL) {
  cli::cli_ul()
  cli::cli_li("Data files:")
  li_files <- cli::cli_ul()
  for(a in 1:length(self$.file_data))
    cli::cli_li(self$.file_data[a])
  cli::cli_end(li_files)
  cli::cli_li(paste("Meta data file:", self$.file_meta))
  cli::cli_end()
}